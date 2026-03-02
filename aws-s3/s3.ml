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
open StdLabels
let sprintf = Printf.sprintf
let xmlm_of_string string =
  let (_dtd, nodes) = Ezxmlm.from_string string in List.hd nodes
let make_xmlm_node ?(ns= "") name attrs nodes : 'a Xmlm.frag=
  `El (((ns, name), attrs), nodes)
module Option =
  struct
    let map ?default ~f = function | None -> default | Some v -> Some (f v)
    let value ~default = function | None -> default | Some v -> v
    let value_map ~default ~f v = (map ~f v) |> (value ~default)
    let value_exn ~message =
      function | Some v -> v | None -> failwith message
  end
let rec filter_map ~f =
  function
  | [] -> []
  | x::xs ->
      (match f x with
       | Some x -> x :: (filter_map ~f xs)
       | None -> filter_map ~f xs)
module Protocol(P:sig type 'a result end) =
  struct
    type time = float
    let time_of_xmlm_exn t =
      try (Protocol_conv_xmlm.Xmlm.to_string t) |> Time.parse_iso8601_string
      with
      | _ ->
          raise
            (let open Protocol_conv_xmlm.Xmlm in
               Protocol_error (make_error ~value:t "Not an iso8601 string"))
    let unquote s =
      match String.length s with
      | 0 | 1 -> s
      | _ when (s.[0]) = '"' ->
          String.sub s ~pos:1 ~len:((String.length s) - 2)
      | _ -> s
    type etag = string
    let etag_of_xmlm_exn t = (Protocol_conv_xmlm.Xmlm.to_string t) |> unquote
    type storage_class =
      | Standard [@key "STANDARD"]
      | Standard_ia [@key "STANDARD_IA"]
      | Onezone_ia [@key "ONEZONE_IA"]
      | Reduced_redundancy [@key "REDUCED_REDUNDANCY"]
      | Glacier [@key "GLACIER"]
    and content =
      {
      storage_class: storage_class [@key "StorageClass"];
      size: int [@key "Size"];
      last_modified: time [@key "LastModified"];
      key: string [@key "Key"];
      etag: etag [@key "ETag"];
      meta_headers: (string * string) list option
        [@default None][@ocaml.doc " Add expiration date option "]}[@@deriving
                                                                    of_protocol
                                                                    ~driver:(module
                                                                    Protocol_conv_xmlm.Xmlm)]
    include
      struct
        let _ = fun (_ : storage_class) -> ()
        let _ = fun (_ : content) -> ()
        let rec (storage_class_of_xmlm_exn :
                  Protocol_conv_xmlm.Xmlm.t -> storage_class)
          =
          Protocol_conv_xmlm.Xmlm.to_variant
            [Protocol_conv.Runtime.Variant_in.Variant
               ("STANDARD", Protocol_conv.Runtime.Tuple_in.Nil, Standard);
            Protocol_conv.Runtime.Variant_in.Variant
              ("STANDARD_IA", Protocol_conv.Runtime.Tuple_in.Nil,
                Standard_ia);
            Protocol_conv.Runtime.Variant_in.Variant
              ("ONEZONE_IA", Protocol_conv.Runtime.Tuple_in.Nil, Onezone_ia);
            Protocol_conv.Runtime.Variant_in.Variant
              ("REDUCED_REDUNDANCY", Protocol_conv.Runtime.Tuple_in.Nil,
                Reduced_redundancy);
            Protocol_conv.Runtime.Variant_in.Variant
              ("GLACIER", Protocol_conv.Runtime.Tuple_in.Nil, Glacier)]
        and (content_of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> content) =
          let f = ref None in
          fun t ->
            match !f with
            | None ->
                let f' =
                  Protocol_conv_xmlm.Xmlm.to_record
                    (let open Protocol_conv.Runtime.Record_in in
                       Cons
                         (("StorageClass", storage_class_of_xmlm_exn, None),
                           (Cons
                              (("Size", Protocol_conv_xmlm.Xmlm.to_int, None),
                                (Cons
                                   (("LastModified", time_of_xmlm_exn, None),
                                     (Cons
                                        (("Key",
                                           Protocol_conv_xmlm.Xmlm.to_string,
                                           None),
                                          (Cons
                                             (("ETag", etag_of_xmlm_exn,
                                                None),
                                               (Cons
                                                  (("meta_headers",
                                                     (Protocol_conv_xmlm.Xmlm.to_option
                                                        (Protocol_conv_xmlm.Xmlm.to_list
                                                           (let spec =
                                                              let open Protocol_conv.Runtime.Tuple_in in
                                                                Cons
                                                                  (Protocol_conv_xmlm.Xmlm.to_string,
                                                                    (
                                                                    Cons
                                                                    (Protocol_conv_xmlm.Xmlm.to_string,
                                                                    Nil))) in
                                                            let constructor
                                                              x0 x1 =
                                                              (x0, x1) in
                                                            Protocol_conv_xmlm.Xmlm.to_tuple
                                                              spec
                                                              constructor))),
                                                     (Some None)), Nil))))))))))))
                    (fun storage_class size last_modified key etag
                       meta_headers ->
                       {
                         storage_class;
                         size;
                         last_modified;
                         key;
                         etag;
                         meta_headers
                       }) in
                (f := (Some f'); f' t)
            | Some f -> f t
        let _ = storage_class_of_xmlm_exn
        and _ = content_of_xmlm_exn
        let (storage_class_of_xmlm :
              Protocol_conv_xmlm.Xmlm.t ->
                (storage_class, Protocol_conv_xmlm.Xmlm.error)
                  Protocol_conv.Runtime.result)
          = Protocol_conv_xmlm.Xmlm.try_with storage_class_of_xmlm_exn
        and (content_of_xmlm :
              Protocol_conv_xmlm.Xmlm.t ->
                (content, Protocol_conv_xmlm.Xmlm.error)
                  Protocol_conv.Runtime.result)
          = Protocol_conv_xmlm.Xmlm.try_with content_of_xmlm_exn
        let _ = storage_class_of_xmlm
        and _ = content_of_xmlm
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    module Ls =
      struct
        type result =
          {
          prefix: string option [@key "Prefix"];
          common_prefixes: string option [@key "CommonPrefixes"];
          delimiter: string option [@key "Delimiter"];
          next_continuation_token: string option
            [@key "NextContinuationToken"];
          name: string [@key "Name"];
          max_keys: int [@key "MaxKeys"];
          key_count: int [@key "KeyCount"];
          is_truncated: bool [@key "IsTruncated"];
          contents: content list [@key "Contents"];
          start_after: string option [@key "StartAfter"]}[@@deriving
                                                           of_protocol
                                                             ~driver:(module
                                                             Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : result) -> ()
            let (result_of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> result) =
              Protocol_conv_xmlm.Xmlm.to_record
                (let open Protocol_conv.Runtime.Record_in in
                   Cons
                     (("Prefix",
                        (Protocol_conv_xmlm.Xmlm.to_option
                           Protocol_conv_xmlm.Xmlm.to_string), None),
                       (Cons
                          (("CommonPrefixes",
                             (Protocol_conv_xmlm.Xmlm.to_option
                                Protocol_conv_xmlm.Xmlm.to_string), None),
                            (Cons
                               (("Delimiter",
                                  (Protocol_conv_xmlm.Xmlm.to_option
                                     Protocol_conv_xmlm.Xmlm.to_string),
                                  None),
                                 (Cons
                                    (("NextContinuationToken",
                                       (Protocol_conv_xmlm.Xmlm.to_option
                                          Protocol_conv_xmlm.Xmlm.to_string),
                                       None),
                                      (Cons
                                         (("Name",
                                            Protocol_conv_xmlm.Xmlm.to_string,
                                            None),
                                           (Cons
                                              (("MaxKeys",
                                                 Protocol_conv_xmlm.Xmlm.to_int,
                                                 None),
                                                (Cons
                                                   (("KeyCount",
                                                      Protocol_conv_xmlm.Xmlm.to_int,
                                                      None),
                                                     (Cons
                                                        (("IsTruncated",
                                                           Protocol_conv_xmlm.Xmlm.to_bool,
                                                           None),
                                                          (Cons
                                                             (("Contents",
                                                                (Protocol_conv_xmlm.Xmlm.to_list
                                                                   content_of_xmlm_exn),
                                                                None),
                                                               (Cons
                                                                  (("StartAfter",
                                                                    (Protocol_conv_xmlm.Xmlm.to_option
                                                                    Protocol_conv_xmlm.Xmlm.to_string),
                                                                    None),
                                                                    Nil))))))))))))))))))))
                (fun prefix common_prefixes delimiter next_continuation_token
                   name max_keys key_count is_truncated contents start_after
                   ->
                   {
                     prefix;
                     common_prefixes;
                     delimiter;
                     next_continuation_token;
                     name;
                     max_keys;
                     key_count;
                     is_truncated;
                     contents;
                     start_after
                   })
            let _ = result_of_xmlm_exn
            let (result_of_xmlm :
                  Protocol_conv_xmlm.Xmlm.t ->
                    (result, Protocol_conv_xmlm.Xmlm.error)
                      Protocol_conv.Runtime.result)
              = Protocol_conv_xmlm.Xmlm.try_with result_of_xmlm_exn
            let _ = result_of_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        type t = (content list * cont) P.result
        and cont =
          | More of (?max_keys:int -> unit -> t) 
          | Done 
      end
    let set_element_name : string -> 'a Xmlm.frag -> 'a Xmlm.frag =
      fun name ->
        function
        | `El (((ns, _), attrs), elems) ->
            make_xmlm_node ~ns name attrs elems
        | `Data _ -> failwith "Not an element"
    module Delete_multi =
      struct
        type objekt =
          {
          key: string [@key "Key"];
          version_id: string option [@key "VersionId"]}[@@deriving
                                                         of_protocol
                                                           ~driver:(module
                                                           Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : objekt) -> ()
            let (objekt_of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> objekt) =
              Protocol_conv_xmlm.Xmlm.to_record
                (let open Protocol_conv.Runtime.Record_in in
                   Cons
                     (("Key", Protocol_conv_xmlm.Xmlm.to_string, None),
                       (Cons
                          (("VersionId",
                             (Protocol_conv_xmlm.Xmlm.to_option
                                Protocol_conv_xmlm.Xmlm.to_string), None),
                            Nil))))
                (fun key version_id -> { key; version_id })
            let _ = objekt_of_xmlm_exn
            let (objekt_of_xmlm :
                  Protocol_conv_xmlm.Xmlm.t ->
                    (objekt, Protocol_conv_xmlm.Xmlm.error)
                      Protocol_conv.Runtime.result)
              = Protocol_conv_xmlm.Xmlm.try_with objekt_of_xmlm_exn
            let _ = objekt_of_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        let objekt_to_xmlm =
          function
          | { key; version_id = None } ->
              make_xmlm_node "object" []
                [make_xmlm_node "Key" [] [`Data key]]
          | { key; version_id = Some version } ->
              make_xmlm_node "object" []
                [make_xmlm_node "Key" [] [`Data key];
                make_xmlm_node "VersionId" [] [`Data version]][@@ocaml.doc
                                                                " We must not transmit the version id at all if not specified "]
        type request =
          {
          quiet: bool [@key "Quiet"];
          objects: objekt list [@key "Object"]}[@@deriving
                                                 to_protocol ~driver:(module
                                                   Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : request) -> ()
            let (request_to_xmlm : request -> Protocol_conv_xmlm.Xmlm.t) =
              let _of_record =
                Protocol_conv_xmlm.Xmlm.of_record
                  (let open Protocol_conv.Runtime.Record_out in
                     Cons
                       (("Quiet", Protocol_conv_xmlm.Xmlm.of_bool, None),
                         (Cons
                            (("Object",
                               (Protocol_conv_xmlm.Xmlm.of_list
                                  objekt_to_xmlm), None), Nil)))) in
              fun { quiet = r_quiet; objects = r_objects } ->
                _of_record r_quiet r_objects
            let _ = request_to_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        let xml_of_request request =
          (request_to_xmlm request) |> (set_element_name "Delete")
        type error =
          {
          key: string [@key "Key"];
          version_id: string option [@key "VersionId"];
          code: string [@key "Code"];
          message: string [@key "Message"]}[@@deriving
                                             of_protocol ~driver:(module
                                               Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : error) -> ()
            let (error_of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> error) =
              Protocol_conv_xmlm.Xmlm.to_record
                (let open Protocol_conv.Runtime.Record_in in
                   Cons
                     (("Key", Protocol_conv_xmlm.Xmlm.to_string, None),
                       (Cons
                          (("VersionId",
                             (Protocol_conv_xmlm.Xmlm.to_option
                                Protocol_conv_xmlm.Xmlm.to_string), None),
                            (Cons
                               (("Code", Protocol_conv_xmlm.Xmlm.to_string,
                                  None),
                                 (Cons
                                    (("Message",
                                       Protocol_conv_xmlm.Xmlm.to_string,
                                       None), Nil))))))))
                (fun key version_id code message ->
                   { key; version_id; code; message })
            let _ = error_of_xmlm_exn
            let (error_of_xmlm :
                  Protocol_conv_xmlm.Xmlm.t ->
                    (error, Protocol_conv_xmlm.Xmlm.error)
                      Protocol_conv.Runtime.result)
              = Protocol_conv_xmlm.Xmlm.try_with error_of_xmlm_exn
            let _ = error_of_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        type delete_marker = bool
        let delete_marker_of_xmlm_exn t =
          (Protocol_conv_xmlm.Xmlm.to_option Protocol_conv_xmlm.Xmlm.to_bool
             t)
            |> ((function | None -> false | Some x -> x))
        type result =
          {
          delete_marker: delete_marker [@key "DeleteMarker"];
          delete_marker_version_id: string option
            [@key "DeleteMarkerVersionId"];
          deleted: objekt list [@key "Deleted"];
          error: error list [@key "Error"]}[@@deriving
                                             of_protocol ~driver:(module
                                               Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : result) -> ()
            let (result_of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> result) =
              Protocol_conv_xmlm.Xmlm.to_record
                (let open Protocol_conv.Runtime.Record_in in
                   Cons
                     (("DeleteMarker", delete_marker_of_xmlm_exn, None),
                       (Cons
                          (("DeleteMarkerVersionId",
                             (Protocol_conv_xmlm.Xmlm.to_option
                                Protocol_conv_xmlm.Xmlm.to_string), None),
                            (Cons
                               (("Deleted",
                                  (Protocol_conv_xmlm.Xmlm.to_list
                                     objekt_of_xmlm_exn), None),
                                 (Cons
                                    (("Error",
                                       (Protocol_conv_xmlm.Xmlm.to_list
                                          error_of_xmlm_exn), None), Nil))))))))
                (fun delete_marker delete_marker_version_id deleted error ->
                   { delete_marker; delete_marker_version_id; deleted; error
                   })
            let _ = result_of_xmlm_exn
            let (result_of_xmlm :
                  Protocol_conv_xmlm.Xmlm.t ->
                    (result, Protocol_conv_xmlm.Xmlm.error)
                      Protocol_conv.Runtime.result)
              = Protocol_conv_xmlm.Xmlm.try_with result_of_xmlm_exn
            let _ = result_of_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
      end
    module Error_response =
      struct
        type t =
          {
          code: string [@key "Code"];
          message: string [@key "Message"];
          bucket: string option [@key "Bucket"];
          endpoint: string option [@key "Endpoint"];
          region: string option [@key "Region"];
          request_id: string [@key "RequestId"];
          host_id: string [@key "HostId"]}[@@deriving
                                            of_protocol ~driver:(module
                                              Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : t) -> ()
            let (of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> t) =
              Protocol_conv_xmlm.Xmlm.to_record
                (let open Protocol_conv.Runtime.Record_in in
                   Cons
                     (("Code", Protocol_conv_xmlm.Xmlm.to_string, None),
                       (Cons
                          (("Message", Protocol_conv_xmlm.Xmlm.to_string,
                             None),
                            (Cons
                               (("Bucket",
                                  (Protocol_conv_xmlm.Xmlm.to_option
                                     Protocol_conv_xmlm.Xmlm.to_string),
                                  None),
                                 (Cons
                                    (("Endpoint",
                                       (Protocol_conv_xmlm.Xmlm.to_option
                                          Protocol_conv_xmlm.Xmlm.to_string),
                                       None),
                                      (Cons
                                         (("Region",
                                            (Protocol_conv_xmlm.Xmlm.to_option
                                               Protocol_conv_xmlm.Xmlm.to_string),
                                            None),
                                           (Cons
                                              (("RequestId",
                                                 Protocol_conv_xmlm.Xmlm.to_string,
                                                 None),
                                                (Cons
                                                   (("HostId",
                                                      Protocol_conv_xmlm.Xmlm.to_string,
                                                      None), Nil))))))))))))))
                (fun code message bucket endpoint region request_id host_id
                   ->
                   {
                     code;
                     message;
                     bucket;
                     endpoint;
                     region;
                     request_id;
                     host_id
                   })
            let _ = of_xmlm_exn
            let (of_xmlm :
                  Protocol_conv_xmlm.Xmlm.t ->
                    (t, Protocol_conv_xmlm.Xmlm.error)
                      Protocol_conv.Runtime.result)
              = Protocol_conv_xmlm.Xmlm.try_with of_xmlm_exn
            let _ = of_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
      end
    module Multipart =
      struct
        type part =
          {
          part_number: int [@key "PartNumber"];
          etag: string [@key "ETag"]}[@@deriving
                                       to_protocol ~driver:(module
                                         Protocol_conv_xmlm.Xmlm)]
        include
          struct
            let _ = fun (_ : part) -> ()
            let (part_to_xmlm : part -> Protocol_conv_xmlm.Xmlm.t) =
              let _of_record =
                Protocol_conv_xmlm.Xmlm.of_record
                  (let open Protocol_conv.Runtime.Record_out in
                     Cons
                       (("PartNumber", Protocol_conv_xmlm.Xmlm.of_int, None),
                         (Cons
                            (("ETag", Protocol_conv_xmlm.Xmlm.of_string,
                               None), Nil)))) in
              fun { part_number = r_part_number; etag = r_etag } ->
                _of_record r_part_number r_etag
            let _ = part_to_xmlm
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        module Initiate =
          struct
            type t =
              {
              bucket: string [@key "Bucket"];
              key: string [@key "Key"];
              upload_id: string [@key "UploadId"]}[@@deriving
                                                    of_protocol
                                                      ~driver:(module
                                                      Protocol_conv_xmlm.Xmlm)]
            include
              struct
                let _ = fun (_ : t) -> ()
                let (of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> t) =
                  Protocol_conv_xmlm.Xmlm.to_record
                    (let open Protocol_conv.Runtime.Record_in in
                       Cons
                         (("Bucket", Protocol_conv_xmlm.Xmlm.to_string, None),
                           (Cons
                              (("Key", Protocol_conv_xmlm.Xmlm.to_string,
                                 None),
                                (Cons
                                   (("UploadId",
                                      Protocol_conv_xmlm.Xmlm.to_string,
                                      None), Nil))))))
                    (fun bucket key upload_id -> { bucket; key; upload_id })
                let _ = of_xmlm_exn
                let (of_xmlm :
                      Protocol_conv_xmlm.Xmlm.t ->
                        (t, Protocol_conv_xmlm.Xmlm.error)
                          Protocol_conv.Runtime.result)
                  = Protocol_conv_xmlm.Xmlm.try_with of_xmlm_exn
                let _ = of_xmlm
              end[@@ocaml.doc "@inline"][@@merlin.hide ]
          end
        module Complete =
          struct
            type request = {
              parts: part list [@key "Part"]}[@@deriving
                                               to_protocol ~driver:(module
                                                 Protocol_conv_xmlm.Xmlm)]
            include
              struct
                let _ = fun (_ : request) -> ()
                let (request_to_xmlm : request -> Protocol_conv_xmlm.Xmlm.t)
                  =
                  let _of_record =
                    Protocol_conv_xmlm.Xmlm.of_record
                      (let open Protocol_conv.Runtime.Record_out in
                         Cons
                           (("Part",
                              (Protocol_conv_xmlm.Xmlm.of_list part_to_xmlm),
                              None), Nil)) in
                  fun { parts = r_parts } -> _of_record r_parts
                let _ = request_to_xmlm
              end[@@ocaml.doc "@inline"][@@merlin.hide ]
            let xml_of_request request =
              (request_to_xmlm request) |>
                (set_element_name "CompleteMultipartUpload")
            type response =
              {
              location: string [@key "Location"];
              bucket: string [@key "Bucket"];
              key: string [@key "Key"];
              etag: string [@key "ETag"]}[@@deriving
                                           of_protocol ~driver:(module
                                             Protocol_conv_xmlm.Xmlm)]
            include
              struct
                let _ = fun (_ : response) -> ()
                let (response_of_xmlm_exn :
                      Protocol_conv_xmlm.Xmlm.t -> response)
                  =
                  Protocol_conv_xmlm.Xmlm.to_record
                    (let open Protocol_conv.Runtime.Record_in in
                       Cons
                         (("Location", Protocol_conv_xmlm.Xmlm.to_string,
                            None),
                           (Cons
                              (("Bucket", Protocol_conv_xmlm.Xmlm.to_string,
                                 None),
                                (Cons
                                   (("Key",
                                      Protocol_conv_xmlm.Xmlm.to_string,
                                      None),
                                     (Cons
                                        (("ETag",
                                           Protocol_conv_xmlm.Xmlm.to_string,
                                           None), Nil))))))))
                    (fun location bucket key etag ->
                       { location; bucket; key; etag })
                let _ = response_of_xmlm_exn
                let (response_of_xmlm :
                      Protocol_conv_xmlm.Xmlm.t ->
                        (response, Protocol_conv_xmlm.Xmlm.error)
                          Protocol_conv.Runtime.result)
                  = Protocol_conv_xmlm.Xmlm.try_with response_of_xmlm_exn
                let _ = response_of_xmlm
              end[@@ocaml.doc "@inline"][@@merlin.hide ]
          end
        module Copy =
          struct
            type t =
              {
              etag: string [@key "ETag"];
              last_modified: time [@key "LastModified"]}[@@deriving
                                                          of_protocol
                                                            ~driver:(module
                                                            Protocol_conv_xmlm.Xmlm)]
            include
              struct
                let _ = fun (_ : t) -> ()
                let (of_xmlm_exn : Protocol_conv_xmlm.Xmlm.t -> t) =
                  Protocol_conv_xmlm.Xmlm.to_record
                    (let open Protocol_conv.Runtime.Record_in in
                       Cons
                         (("ETag", Protocol_conv_xmlm.Xmlm.to_string, None),
                           (Cons
                              (("LastModified", time_of_xmlm_exn, None), Nil))))
                    (fun etag last_modified -> { etag; last_modified })
                let _ = of_xmlm_exn
                let (of_xmlm :
                      Protocol_conv_xmlm.Xmlm.t ->
                        (t, Protocol_conv_xmlm.Xmlm.error)
                          Protocol_conv.Runtime.result)
                  = Protocol_conv_xmlm.Xmlm.try_with of_xmlm_exn
                let _ = of_xmlm
              end[@@ocaml.doc "@inline"][@@merlin.hide ]
          end
      end
  end
module Make(Io:Types.Io) =
  struct
    module Aws = (Aws.Make)(Io)
    module Body = (Body.Make)(Io)
    open Io
    open Deferred
    type error =
      | Redirect of Region.endpoint 
      | Throttled 
      | Unknown of int * string 
      | Failed of exn 
      | Forbidden 
      | Not_found 
    let string_sink () =
      let (reader, writer) = Pipe.create () in
      ((Body.to_string reader), writer)
    include
      (Protocol)(struct
                   type nonrec 'a result = ('a, error) result Deferred.t
                 end)
    type range = {
      first: int option ;
      last: int option }
    type nonrec 'a result = ('a, error) result Deferred.t
    type 'a command =
      ?credentials:Credentials.t ->
        ?connect_timeout_ms:int ->
          ?confirm_requester_pays:bool -> endpoint:Region.endpoint -> 'a
    let maybe_add_request_payer confirm_requester_pays headers =
      match confirm_requester_pays with
      | true -> ("x-amz-request-payer", "requester") :: headers
      | false -> headers[@@ocaml.doc
                          " Conditionally add a header indicating caller's willingness to\n     pay for AWS data transfer costs. This only applies to buckets\n     configured to limit access to paying requesters. "]
    [@@@ocaml.text "/*"]
    let do_command ~endpoint:(endpoint : Region.endpoint) cmd =
      ((cmd ()) >>=
         (function
          | Ok v -> return (Ok v)
          | Error exn -> return (Error (Failed exn))))
        >>=?
        (fun (code, _message, headers, body) ->
           match code with
           | code when (200 <= code) && (code < 300) ->
               Deferred.return (Ok headers)
           | 403 -> Deferred.return (Error Forbidden)
           | 404 -> Deferred.return (Error Not_found)
           | c when (300 <= c) && (c < 400) ->
               let region =
                 Region.of_string
                   (Headers.find "x-amz-bucket-region" headers) in
               Deferred.return (Error (Redirect { endpoint with region }))
           | c when (400 <= c) && (c < 500) ->
               let open Error_response in
                 let xml = xmlm_of_string body in
                 (match Error_response.of_xmlm_exn xml with
                  | { code = "PermanentRedirect"; endpoint = Some host;_}
                    | { code = "TemporaryRedirect"; endpoint = Some host;_}
                      ->
                      let region = Region.of_host host in
                      Deferred.return
                        (Error (Redirect { endpoint with region }))
                  | { code = "AuthorizationHeaderMalformed";
                      region = Some region;_} ->
                      let region = Region.of_string region in
                      Deferred.return
                        (Error (Redirect { endpoint with region }))
                  | { code;_} -> Deferred.return (Error (Unknown (c, code))))
           | 500 | 503 -> Deferred.return (Error Throttled)
           | code ->
               let resp = Error_response.of_xmlm_exn (xmlm_of_string body) in
               Deferred.return (Error (Unknown (code, resp.code))))
    let put_common ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
      false) ~endpoint ?content_type ?content_encoding ?acl ?cache_control
      ?expect ?(meta_headers= []) ~bucket ~key ~body () =
      let path = sprintf "/%s/%s" bucket key in
      let headers =
        ((([("Content-Type", content_type);
           ("Content-Encoding", content_encoding);
           ("Cache-Control", cache_control);
           ("x-amz-acl", acl)] |>
             (List.filter
                ~f:(function | (_, Some _) -> true | (_, None) -> false)))
            |>
            (List.map
               ~f:(function
                   | (k, Some v) -> (k, v)
                   | (_, None) -> failwith "Impossible")))
           |>
           (List.rev_append
              (meta_headers |>
                 (List.map
                    ~f:(fun (k, v) -> ((Printf.sprintf "x-amz-meta-%s" k), v))))))
          |> (maybe_add_request_payer confirm_requester_pays) in
      let sink = Body.null () in
      let cmd () =
        Aws.make_request ~endpoint ?expect ?credentials ?connect_timeout_ms
          ~headers ~meth:`PUT ~path ~sink ~body ~query:[] () in
      (do_command ~endpoint cmd) >>=?
        (fun headers ->
           let etag =
             match Headers.find_opt "etag" headers with
             | None -> failwith "Put reply did not contain an etag header"
             | Some etag -> unquote etag in
           Deferred.return (Ok etag))
    [@@@ocaml.text "/*"]
    module Stream =
      struct
        let get ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
          false) ~endpoint ?range ~bucket ~key ~data () =
          let headers =
            let r_opt = function | Some r -> string_of_int r | None -> "" in
            match range with
            | Some { first = None; last = None } -> []
            | Some { first = Some first; last } ->
                [("Range", (sprintf "bytes=%d-%s" first (r_opt last)))]
            | Some { first = None; last = Some last } when last < 0 ->
                [("Range", (sprintf "bytes=-%d" last))]
            | Some { first = None; last = Some last } ->
                [("Range", (sprintf "bytes=0-%d" last))]
            | None -> [] in
          let headers =
            maybe_add_request_payer confirm_requester_pays headers in
          let path = sprintf "/%s/%s" bucket key in
          let cmd () =
            Aws.make_request ~endpoint ?credentials ?connect_timeout_ms
              ~sink:data ~headers ~meth:`GET ~path ~query:[] () in
          (do_command ~endpoint cmd) >>=?
            (fun _headers -> Deferred.return (Ok ()))
        let put ?credentials ?connect_timeout_ms ?confirm_requester_pays
          ~endpoint ?content_type ?content_encoding ?acl ?cache_control
          ?expect ?meta_headers ~bucket ~key ~data ~chunk_size ~length () =
          let body = Body.Chunked { length; chunk_size; pipe = data } in
          put_common ~endpoint ?credentials ?connect_timeout_ms
            ?confirm_requester_pays ?content_type ?content_encoding ?acl
            ?cache_control ?expect ?meta_headers ~bucket ~key ~body ()
      end
    let put ?credentials ?connect_timeout_ms ?confirm_requester_pays
      ~endpoint ?content_type ?content_encoding ?acl ?cache_control ?expect
      ?meta_headers ~bucket ~key ~data () =
      let body = Body.String data in
      put_common ?credentials ?connect_timeout_ms ?confirm_requester_pays
        ?content_type ?content_encoding ?acl ?cache_control ?expect
        ?meta_headers ~endpoint ~bucket ~key ~body ()
    let get ?credentials ?connect_timeout_ms ?confirm_requester_pays
      ~endpoint ?range ~bucket ~key () =
      let (body, data) = string_sink () in
      (Stream.get ?credentials ?connect_timeout_ms ?confirm_requester_pays
         ~endpoint ?range ~bucket ~key ~data ())
        >>=? (fun () -> body >>= (fun body -> Deferred.return (Ok body)))
    let delete ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
      false) ~endpoint ~bucket ~key () =
      let path = sprintf "/%s/%s" bucket key in
      let sink = Body.null () in
      let headers = maybe_add_request_payer confirm_requester_pays [] in
      let cmd () =
        Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers
          ~meth:`DELETE ~path ~query:[] ~sink () in
      (do_command ~endpoint cmd) >>=?
        (fun _headers -> Deferred.return (Ok ()))
    let head ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
      false) ~endpoint ~bucket ~key () =
      let path = sprintf "/%s/%s" bucket key in
      let sink = Body.null () in
      let headers = maybe_add_request_payer confirm_requester_pays [] in
      let cmd () =
        Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers
          ~meth:`HEAD ~path ~query:[] ~sink () in
      (do_command ~endpoint cmd) >>=?
        (fun headers ->
           let result =
             let (>>=) a f = match a with | Some x -> f x | None -> None in
             (Headers.find_opt "content-length" headers) >>=
               (fun size ->
                  (Headers.find_opt "etag" headers) >>=
                    (fun etag ->
                       (Headers.find_opt "last-modified" headers) >>=
                         (fun last_modified ->
                            let meta_headers =
                              Headers.find_prefix ~prefix:"x-amz-meta-"
                                headers in
                            let last_modified =
                              Time.parse_rcf1123_string last_modified in
                            let size = size |> int_of_string in
                            let storage_class =
                              (Headers.find_opt "x-amz-storage-class" headers)
                                |>
                                (Option.value_map ~default:Standard
                                   ~f:(fun s ->
                                         storage_class_of_xmlm_exn
                                           (make_xmlm_node "p" [] [`Data s]))) in
                            Some
                              {
                                storage_class;
                                size;
                                last_modified;
                                key;
                                etag = (unquote etag);
                                meta_headers = (Some meta_headers)
                              }))) in
           match result with
           | Some r -> Deferred.return (Ok r)
           | None ->
               Deferred.return
                 (Error
                    (Unknown (1, "Result did not return correct headers"))))
    let delete_multi ?credentials ?connect_timeout_ms
      ?(confirm_requester_pays= false) ~endpoint ~bucket ~objects () =
      match objects with
      | [] ->
          (let open Delete_multi in
             {
               delete_marker = false;
               delete_marker_version_id = None;
               deleted = [];
               error = []
             })
            |> ((fun r -> Deferred.return (Ok r)))
      | _ ->
          let request =
            ((let open Delete_multi in { quiet = false; objects }) |>
               Delete_multi.xml_of_request)
              |> (fun req -> Ezxmlm.to_string [req]) in
          let headers =
            [("Content-MD5",
               (Base64.encode_string (Stdlib.Digest.string request)))] in
          let headers =
            maybe_add_request_payer confirm_requester_pays headers in
          let (body, sink) = string_sink () in
          let cmd () =
            Aws.make_request ~endpoint ~body:(Body.String request)
              ?credentials ?connect_timeout_ms ~headers ~meth:`POST
              ~query:[("delete", "")] ~path:("/" ^ bucket) ~sink () in
          (do_command ~endpoint cmd) >>=?
            ((fun _headers ->
                body >>=
                  (fun body ->
                     let result =
                       Delete_multi.result_of_xmlm_exn (xmlm_of_string body) in
                     Deferred.return (Ok result))))
    let rec ls ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
      false) ~endpoint ?start_after ?continuation_token ?prefix ?max_keys
      ~bucket () =
      let max_keys =
        match max_keys with | Some n when n > 1000 -> None | n -> n in
      let query =
        [Some ("list-type", "2");
        Option.map ~f:(fun ct -> ("continuation-token", ct))
          continuation_token;
        Option.map ~f:(fun prefix -> ("prefix", prefix)) prefix;
        Option.map
          ~f:(fun max_keys -> ("max-keys", (string_of_int max_keys)))
          max_keys;
        Option.map ~f:(fun start_after -> ("start-after", start_after))
          start_after] |> (filter_map ~f:(fun x -> x)) in
      let headers = maybe_add_request_payer confirm_requester_pays [] in
      let (body, sink) = string_sink () in
      let cmd () =
        Aws.make_request ?credentials ?connect_timeout_ms ~endpoint ~headers
          ~meth:`GET ~path:("/" ^ bucket) ~query ~sink () in
      (do_command ~endpoint cmd) >>=?
        (fun _headers ->
           body >>=
             (fun body ->
                let result = Ls.result_of_xmlm_exn (xmlm_of_string body) in
                let continuation =
                  match let open Ls in result.next_continuation_token with
                  | Some ct ->
                      Ls.More
                        (ls ?credentials ?connect_timeout_ms
                           ?start_after:None ~continuation_token:ct ?prefix
                           ~confirm_requester_pays ~endpoint ~bucket)
                  | None -> Ls.Done in
                Deferred.return
                  (Ok (let open Ls in (result.contents, continuation)))))
      [@@ocaml.doc " List contents of bucket in s3. "]
    module Multipart_upload =
      struct
        type t =
          {
          id: string ;
          mutable parts: Multipart.part list ;
          bucket: string ;
          key: string }
        let make ~bucket ~key ~upload_id ~parts =
          let multipart_parts =
            List.map parts
              ~f:(fun (part_number, etag) ->
                    { Multipart.part_number = part_number; etag }) in
          { id = upload_id; parts = multipart_parts; bucket; key }[@@ocaml.doc
                                                                    " Create a multipart upload object from explicit parameters.\n        This is useful for stateless workflows where upload metadata is stored in a database. "]
        let init ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
          false) ~endpoint ?content_type ?content_encoding ?acl
          ?cache_control ~bucket ~key () =
          let path = sprintf "/%s/%s" bucket key in
          let query = [("uploads", "")] in
          let headers =
            let content_type =
              Option.map ~f:(fun ct -> ("Content-Type", ct)) content_type in
            let cache_control =
              Option.map ~f:(fun cc -> ("Cache-Control", cc)) cache_control in
            let acl = Option.map ~f:(fun acl -> ("x-amz-acl", acl)) acl in
            filter_map ~f:(fun x -> x)
              [content_type; content_encoding; cache_control; acl] in
          let headers =
            maybe_add_request_payer confirm_requester_pays headers in
          let (body, sink) = string_sink () in
          let cmd () =
            Aws.make_request ?credentials ?connect_timeout_ms ~endpoint
              ~headers ~meth:`POST ~path ~query ~sink () in
          (do_command ~endpoint cmd) >>=?
            (fun _headers ->
               body >>=
                 (fun body ->
                    let resp =
                      Multipart.Initiate.of_xmlm_exn (xmlm_of_string body) in
                    (Ok
                       {
                         id = (resp.Multipart.Initiate.upload_id);
                         parts = [];
                         bucket;
                         key
                       })
                      |> Deferred.return))[@@ocaml.doc
                                            " Initiate a multipart upload "]
        let upload_part ?credentials ?connect_timeout_ms
          ?(confirm_requester_pays= false) ~endpoint t ~part_number ?expect
          ~data () =
          let path = sprintf "/%s/%s" t.bucket t.key in
          let query =
            [("partNumber", (string_of_int part_number)); ("uploadId", t.id)] in
          let sink = Body.null () in
          let headers = maybe_add_request_payer confirm_requester_pays [] in
          let cmd () =
            Aws.make_request ?expect ?credentials ?connect_timeout_ms
              ~endpoint ~headers ~meth:`PUT ~path ~body:(Body.String data)
              ~query ~sink () in
          (do_command ~endpoint cmd) >>=?
            (fun headers ->
               let etag =
                 ((Headers.find_opt "etag" headers) |>
                    (fun etag ->
                       Option.value_exn
                         ~message:"Put reply did not contain an etag header"
                         etag))
                   |> (fun etag -> unquote etag) in
               t.parts <- ({ etag; part_number } :: (t.parts));
               Deferred.return (Ok ()))[@@ocaml.doc
                                         " Upload a part of the file.\n        Parts must be at least 5Mb except for the last part\n        [part_number] specifies the part numer. Parts will be assembled in order, but\n        does not have to be consecutive\n    "]
        let copy_part ?credentials ?connect_timeout_ms
          ?(confirm_requester_pays= false) ~endpoint t ~part_number ?range
          ~bucket ~key () =
          let path = sprintf "/%s/%s" t.bucket t.key in
          let query =
            [("partNumber", (string_of_int part_number)); ("uploadId", t.id)] in
          let headers = ("x-amz-copy-source", (sprintf "/%s/%s" bucket key))
            ::
            (Option.value_map ~default:[]
               ~f:(fun (first, last) ->
                     [("x-amz-copy-source-range",
                        (sprintf "bytes=%d-%d" first last))]) range) in
          let headers =
            maybe_add_request_payer confirm_requester_pays headers in
          let (body, sink) = string_sink () in
          let cmd () =
            Aws.make_request ?credentials ?connect_timeout_ms ~endpoint
              ~headers ~meth:`PUT ~path ~query ~sink () in
          (do_command ~endpoint cmd) >>=?
            (fun _headers ->
               body >>=
                 (fun body ->
                    let xml = xmlm_of_string body in
                    match Multipart.Copy.of_xmlm_exn xml with
                    | { Multipart.Copy.etag = etag;_} ->
                        (t.parts <- ({ etag; part_number } :: (t.parts));
                         Deferred.return (Ok ()))))[@@ocaml.doc
                                                     " Specify a part to be a file on s3.\n        [range] can be used to only include a part of the s3 file\n    "]
        let complete ?credentials ?connect_timeout_ms
          ?(confirm_requester_pays= false) ~endpoint t () =
          let path = sprintf "/%s/%s" t.bucket t.key in
          let query = [("uploadId", t.id)] in
          let request =
            let sorted_parts =
              Stdlib.List.sort
                (fun a b -> compare a.Multipart.part_number b.part_number)
                t.parts in
            (let open Multipart.Complete in
               xml_of_request { parts = sorted_parts })
              |> (fun node -> Format.asprintf "%a" Ezxmlm.pp [node]) in
          let (body, sink) = string_sink () in
          let headers = maybe_add_request_payer confirm_requester_pays [] in
          let cmd () =
            Aws.make_request ?credentials ?connect_timeout_ms ~endpoint
              ~headers ~meth:`POST ~path ~query ~body:(Body.String request)
              ~sink () in
          (do_command ~endpoint cmd) >>=?
            (fun _headers ->
               body >>=
                 (fun body ->
                    let xml = xmlm_of_string body in
                    match Multipart.Complete.response_of_xmlm_exn xml with
                    | { location = _; etag; bucket = resp_bucket;
                        key = resp_key } when
                        (resp_bucket = t.bucket) && (resp_key = t.key) ->
                        (Ok etag) |> Deferred.return
                    | _ ->
                        (Error (Unknown ((-1), "Bucket/key does not match")))
                          |> Deferred.return))[@@ocaml.doc
                                                " Complete the multipart upload.\n      The returned etag is a opaque identifier (not md5)\n    "]
        let abort ?credentials ?connect_timeout_ms ?(confirm_requester_pays=
          false) ~endpoint t () =
          let path = sprintf "/%s/%s" t.bucket t.key in
          let query = [("uploadId", t.id)] in
          let sink = Body.null () in
          let headers = maybe_add_request_payer confirm_requester_pays [] in
          let cmd () =
            Aws.make_request ?credentials ~endpoint ?connect_timeout_ms
              ~headers ~meth:`DELETE ~path ~query ~sink () in
          (do_command ~endpoint cmd) >>=?
            (fun _headers -> Deferred.return (Ok ()))[@@ocaml.doc
                                                       " Abort a multipart upload, deleting all specified parts "]
        let get_upload_id t = t.id[@@ocaml.doc " Accessor functions "]
        let get_bucket t = t.bucket
        let get_key t = t.key
        module Stream =
          struct
            let upload_part ?credentials ?connect_timeout_ms
              ?(confirm_requester_pays= false) ~endpoint t ~part_number
              ?expect ~data ~length ~chunk_size () =
              let path = sprintf "/%s/%s" t.bucket t.key in
              let query =
                [("partNumber", (string_of_int part_number));
                ("uploadId", t.id)] in
              let body = Body.Chunked { length; chunk_size; pipe = data } in
              let sink = Body.null () in
              let headers = maybe_add_request_payer confirm_requester_pays [] in
              let cmd () =
                Aws.make_request ?expect ?credentials ?connect_timeout_ms
                  ~endpoint ~headers ~meth:`PUT ~path ~body ~query ~sink () in
              (do_command ~endpoint cmd) >>=?
                (fun headers ->
                   let etag =
                     ((Headers.find_opt "etag" headers) |>
                        (fun etag ->
                           Option.value_exn
                             ~message:"Put reply did not contain an etag header"
                             etag))
                       |>
                       (fun etag ->
                          String.sub ~pos:1 ~len:((String.length etag) - 2)
                            etag) in
                   t.parts <- ({ etag; part_number } :: (t.parts));
                   Deferred.return (Ok ()))
          end
      end[@@ocaml.doc " Function for doing multipart uploads "]
    let retry ~endpoint ~retries ~f () =
      let delay n =
        let jitter = (Random.float 0.5) +. 0.5 in
        let backoff = 2.0 ** (float n) in (min 60.0 backoff) *. jitter in
      let rec inner ~endpoint ~retry_count ~redirected () =
        (f ~endpoint ()) >>=
          ((function
            | Error (Redirect _) as e when redirected -> Deferred.return e
            | Error (Redirect endpoint) ->
                inner ~endpoint ~retry_count ~redirected:true ()
            | Error _ as e when retry_count = retries -> Deferred.return e
            | Error (Throttled) ->
                (Deferred.after (delay (retry_count + 1))) >>=
                  ((fun () ->
                      inner ~endpoint ~retry_count:(retry_count + 1)
                        ~redirected ()))
            | Error _ ->
                inner ~endpoint ~retry_count:(retry_count + 1) ~redirected ()
            | Ok r -> Deferred.return (Ok r))) in
      inner ~endpoint ~retry_count:0 ~redirected:false ()
  end
