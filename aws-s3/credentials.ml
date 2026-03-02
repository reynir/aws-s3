[@@@ocaml.ppx.context
  {
    tool_name = "ppx_driver";
    include_dirs = [];
    hidden_include_dirs = [];
    load_path = ([], []);
    open_modules = [];
    for_package = None;
    debug = false;
    use_threads = false;
    use_vmthreads = false;
    recursive_types = false;
    principal = false;
    no_alias_deps = false;
    unboxed_types = false;
    unsafe_string = false;
    cookies = [("library-name", "aws_s3")]
  }]
open! StdLabels[@@warning "-66"]
let sprintf = Printf.sprintf
open Protocol_conv_json
type time = float
let time_of_json_exn t =
  try (Json.to_string t) |> Time.parse_iso8601_string
  with
  | _ ->
      raise
        (let open Json in
           Protocol_error (make_error ~value:t "Not an iso8601 string"))
type t =
  {
  access_key: string [@key "AccessKeyId"];
  secret_key: string [@key "SecretAccessKey"];
  token: string option [@key "Token"];
  expiration: time option [@key "Expiration"]}[@@deriving
                                                of_protocol ~driver:(module
                                                  Json)]
include
  struct
    let _ = fun (_ : t) -> ()
    let (of_json_exn : Json.t -> t) =
      Json.to_record
        (let open Protocol_conv.Runtime.Record_in in
           Cons
             (("AccessKeyId", Json.to_string, None),
               (Cons
                  (("SecretAccessKey", Json.to_string, None),
                    (Cons
                       (("Token", (Json.to_option Json.to_string), None),
                         (Cons
                            (("Expiration",
                               (Json.to_option time_of_json_exn), None), Nil))))))))
        (fun access_key secret_key token expiration ->
           { access_key; secret_key; token; expiration })
    let _ = of_json_exn
    let (of_json : Json.t -> (t, Json.error) Protocol_conv.Runtime.result) =
      Json.try_with of_json_exn
    let _ = of_json
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let make ~access_key ~secret_key ?token ?expiration () =
  { access_key; secret_key; token; expiration }
module Make(Io:Types.Io) =
  struct
    module Http = (Http.Make)(Io)
    module Body = (Body.Make)(Io)
    open Io
    open Deferred
    module Iam =
      struct
        let instance_data_endpoint =
          let instance_data_host = "instance-data.ec2.internal" in
          let instance_region = Region.Other instance_data_host in
          Region.endpoint ~inet:`V4 ~scheme:`Http instance_region
        let get_role () =
          let path = "/latest/meta-data/iam/security-credentials/" in
          let (body, sink) =
            let (reader, writer) = Pipe.create () in
            ((Body.to_string reader), writer) in
          (Http.call ~endpoint:instance_data_endpoint ~path ~sink
             ~headers:Headers.empty `GET)
            >>=?
            (fun (status, message, _headers, error_body) ->
               match status with
               | code when (code >= 200) && (code < 300) ->
                   body >>= ((fun body -> Deferred.Or_error.return body))
               | _ ->
                   let msg =
                     sprintf "Failed to get role. %s. Reponse %s" message
                       error_body in
                   Deferred.Or_error.fail (Failure msg))
        let get_credentials role =
          let path =
            sprintf "/latest/meta-data/iam/security-credentials/%s" role in
          let (body, sink) =
            let (reader, writer) = Pipe.create () in
            ((Body.to_string reader), writer) in
          (Http.call ~endpoint:instance_data_endpoint ~path ~sink
             ~headers:Headers.empty `GET)
            >>=?
            (fun (status, message, _headers, error_body) ->
               match status with
               | code when (code >= 200) && (code < 300) ->
                   body >>=
                     ((fun body ->
                         let json = Yojson.Safe.from_string body in
                         Deferred.Or_error.catch
                           (fun () ->
                              (of_json_exn json) |> Deferred.Or_error.return)))
               | _ ->
                   let msg =
                     sprintf "Failed to get credentials. %s. Reponse %s"
                       message error_body in
                   Deferred.Or_error.fail (Failure msg))
      end
    module Helper =
      struct
        let get_credentials ?profile:_ () =
          (Iam.get_role ()) >>=? (fun role -> Iam.get_credentials role)
      end
  end
