open Lwt.Infix

module Log =
  (val Logs.(src_log (Src.create ~doc:"AWS-S3 Mirage" "aws-s3-mirage"))
    : Logs.LOG)


module Make(Flow : Mirage_flow.S)(He : Happy_eyeballs_mirage.S with type flow = Flow.flow) = struct

  module Tls_flow = Tls_mirage.Make(Flow)

  module Deferred = struct
    type 'a t = 'a Lwt.t

    module Or_error = struct
      type nonrec 'a t = ('a, exn) Lwt_result.t
      let return = Lwt_result.return
      let fail = Lwt_result.fail
      let catch : (unit -> 'a t) -> 'a t = fun f ->
        Lwt.catch f Lwt_result.fail

      let (>>=) a b =
        a >>= function
        | Ok v -> b v
        | Error _ as err -> Lwt.return err
    end

    let (>>=) = Lwt.Infix.(>>=)
    let (>>|) a b = a >>= fun v -> Lwt.return (b v)
    let (>>=?) = Lwt_result.Infix.(>>=)

    let return = Lwt.return
    let after delay =
      Mirage_sleep.ns (Duration.of_f delay)

    let catch f =
      Lwt.catch
        (fun () -> f () >>= Or_error.return)
        (fun exn -> Or_error.fail exn)
    let async t = Lwt.async (fun () -> t)
  end

  module Ivar = struct
    type 'a t = ('a Lwt.t * 'a Lwt.u)
    let create () = Lwt.wait ()
    let fill t v = Lwt.wakeup_later (snd t) v
    let wait t = fst t
  end

  module Pipe = struct
    open Lwt.Infix
    type 'a elem = Flush of unit Lwt.u
                 | Data of 'a

    type writer_phantom = [`Writer]
    type reader_phantom = [`Reader]

    type ('a, 'phantom) pipe =
      { cond: unit Lwt_condition.t;
        queue: 'a elem Queue.t;
        mutable closed: bool;
        closer: (unit Lwt.t * unit Lwt.u);
      }

    type 'a reader = ('a, reader_phantom) pipe
    type 'a writer = ('a, writer_phantom) pipe

    let on_close pipe =
      match Lwt.is_sleeping (fst pipe.closer) with
      | true -> Lwt.wakeup_later (snd pipe.closer) ()
      | false -> ()

    let write writer data =
      match writer.closed with
      | false ->
        Queue.add (Data data) writer.queue;
        if Queue.length writer.queue = 1 then Lwt_condition.signal writer.cond ();
        Lwt.return_unit
      | true ->
        failwith (__LOC__ ^ ": Closed")

    let flush writer =
      match Queue.length writer.queue = 0 && writer.closed with
      | true -> Lwt.return ()
      | false ->
        let waiter, wakeup = Lwt.wait () in
        Queue.add (Flush wakeup) writer.queue;
        if Queue.length writer.queue = 1 then Lwt_condition.signal writer.cond ();
        waiter

    let close (writer : 'a writer) =
      writer.closed <- true;
      Lwt_condition.broadcast writer.cond ();
      on_close writer

    let close_reader (reader : 'a reader) =
      reader.closed <- true;
      Lwt_condition.broadcast reader.cond ();
      on_close reader

    let rec read (reader : 'a reader) =
      match Queue.take reader.queue with
      | Data data -> Lwt.return (Some data)
      | Flush wakeup ->
        Lwt.wakeup_later wakeup ();
        read reader
      | exception Queue.Empty when reader.closed ->
        Lwt.return None
      | exception Queue.Empty ->
        Lwt_condition.wait reader.cond >>= fun () ->
        read reader

    let create : unit -> 'a reader * 'a writer = fun () ->
      let pipe = { cond = Lwt_condition.create ();
                   queue = Queue.create ();
                   closed = false;
                   closer = Lwt.wait ();
                 }
      in
      pipe, pipe

    let create_reader: f:('a writer -> unit Lwt.t) -> 'a reader = fun ~f ->
      let reader, writer = create () in
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> f writer)
            (fun _ -> Printf.eprintf "Create_reader raised\n%!"; Lwt.return ()) >>= fun () ->
          close_reader reader; Lwt.return ()
        );
      reader

    let create_writer: f:('a reader -> unit Lwt.t) -> 'a writer = fun ~f ->
      let reader, writer = create () in
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> f reader)
            (fun _ -> Printf.eprintf "Create_writer raised\n%!"; Lwt.return ()) >>= fun () ->
          close writer; Lwt.return ()
        );
      writer

    let is_closed pipe = pipe.closed
    let closed pipe = fst pipe.closer

    (* If the writer is closed, so is the reader *)
    let rec transfer reader writer =
      match is_closed writer with
      | true ->
        Printf.eprintf "Writer closed early\n%!";
        close_reader reader;
        Lwt.return ()
      | false -> begin
          match Queue.take reader.queue with
          | v ->
            Queue.push v writer.queue;
            if Queue.length writer.queue = 1 then Lwt_condition.signal writer.cond ();
            transfer reader writer
          | exception Queue.Empty when reader.closed ->
            Lwt.return ();
          | exception Queue.Empty ->
            Lwt_condition.wait reader.cond >>= fun () ->
            transfer reader writer
        end
  end

  module Net = struct
    open Lwt.Syntax

    let buffer_size = 4096

    let pipes_of_flow flow =
      let reader, input = Pipe.create () in
      let rec read () =
        let* r = Flow.read flow in
        match input.Pipe.closed, r with
        | _, Ok `Eof ->
          Pipe.close input;
          Lwt.return ()
        | _, Error e ->
          Log.debug (fun m -> m "read error %a" Flow.pp_error e);
          Pipe.close input;
          Lwt.return ()
        | true, _ ->
          Lwt.return ()
        | false, Ok `Data data ->
          let* () = Pipe.write input (Cstruct.to_string data) in
          read ()
      in
      (* We close input and output when input is closed *)
      Lwt.async (fun () -> Pipe.closed reader >>= fun () ->
                  Flow.close flow);
      Lwt.async read;

      let output, writer = Pipe.create () in
      let rec write () =
        match Queue.take output.Pipe.queue with
        | Flush waiter ->
          (* Lwt_io.flush oc >>= fun () -> *)
          Lwt.wakeup_later waiter ();
          write ()
        | Data data ->
          let* r = Flow.write flow (Cstruct.of_string data) in
          (match r with
           | Error e -> Fmt.kstr failwith "Flow.write: %a" Flow.pp_write_error e
           | Ok () ->
             write ())
        | exception Queue.Empty when output.Pipe.closed ->
          Lwt.return ()
        | exception Queue.Empty ->
          Lwt_condition.wait output.Pipe.cond >>= fun () ->
          write ()
      in
      Lwt.async write;
      Deferred.Or_error.return (reader, writer)

    let pipes_of_tls_flow flow =
      ignore flow; assert false

    let connect
        ?tls_authenticator:(authenticator = Result.get_ok @@ Ca_certs_nss.authenticator ())
        he
        ?connect_timeout_ms ~inet:_ ~host ~port ~scheme () =
      let tls_config = Result.get_ok @@ Tls.Config.client ~authenticator () in
      ignore connect_timeout_ms;
      let* r =
        match 
          Result.bind (Domain_name.of_string host)
            Domain_name.host
        with
        | Ok host ->
          He.connect_host he host [ port ]
        | Error _ ->
          He.connect_ip he [ Ipaddr.of_string_exn host, port ]
      in
      match r with
      | Error `Msg e -> failwith ("He.connect_host: " ^ e)
      | Ok ((_ip, _port), flow) ->
        match scheme with
        | `Http ->
          pipes_of_flow flow
        | `Https ->
          let host =
            Result.bind (Domain_name.of_string host)
              Domain_name.host
            |> Result.to_option
          in
          let* r = Tls_flow.client_of_flow tls_config ?host flow in
          match r with
          | Error e -> failwith (Fmt.str "Tls_flow.client_of_flow: %a" Tls_flow.pp_write_error e)
          | Ok flow ->
            Lwt.return (Obj.magic flow)
  end
end
