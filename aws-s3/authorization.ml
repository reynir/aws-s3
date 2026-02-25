(* Auth based on aws papers
   https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
*)
open StdLabels
let sprintf = Printf.sprintf

let debug = false
let log fmt = match debug with
  | true -> Printf.kfprintf (fun _ -> ()) stderr ("%s: " ^^ fmt ^^ "\n%!") __MODULE__
  | false -> Printf.ikfprintf (fun _ -> ()) stderr fmt

let hash_sha256 s =
  Digestif.SHA256.digest_string s

let hmac_sha256 ~key v =
  Digestif.SHA256.hmac_string ~key v

let to_raw sha256 = Digestif.SHA256.to_raw_string sha256

let to_hex str = Digestif.SHA256.to_hex str

let make_signing_key =
  let cache = Hashtbl.create 0 in
  fun ?(bypass_cache=false) ~date ~region ~credentials ~service () ->
    match Hashtbl.find_opt cache (credentials.Credentials.access_key, region) with
    | Some (d, signing_key) when d = date && not bypass_cache -> signing_key
    | Some _ | None ->
      let date_key = hmac_sha256 ~key:("AWS4" ^ credentials.Credentials.secret_key) date in
      let date_region_key = hmac_sha256 ~key:(to_raw date_key) region in
      let date_region_service_key = hmac_sha256 ~key:(to_raw date_region_key) service in
      let signing_key = hmac_sha256 ~key:(to_raw date_region_service_key) "aws4_request" in
      Hashtbl.replace cache (credentials.Credentials.access_key, region) (date, signing_key);
      signing_key

let make_scope ~date ~region ~service =
  sprintf "%s/%s/%s/aws4_request" date region service

let string_to_sign ~date ~time ~verb ~path ~query ~headers ~payload_sha ~scope =
  assert (Headers.cardinal headers > 0);
  (* Count sizes of headers *)
  let (key_size, value_size) =
    Headers.fold (
      fun key data (h, v) -> (h + String.length key, v + String.length data)
    ) headers (0,0)
  in
  let header_count = Headers.cardinal headers in
  let canonical_headers = Buffer.create (key_size + value_size + (2 (*:\n*) * header_count)) in
  let signed_headers = Buffer.create (key_size + (Headers.cardinal headers - 1)) in

  let first = ref true in
  Headers.iter (fun key data ->
      let lower_header = String.lowercase_ascii key in
      if (not !first) then Buffer.add_string signed_headers ";";
      Buffer.add_string signed_headers lower_header;
      Buffer.add_string canonical_headers lower_header;
      Buffer.add_string canonical_headers ":";
      Buffer.add_string canonical_headers data;
      Buffer.add_string canonical_headers "\n";
      first := false;
    ) headers;

  (* Strip the trailing from signed_headers *)
  let signed_headers = Buffer.contents signed_headers in
  let canonical_query =
    query
    |> List.map ~f:(fun (k, v) -> sprintf "%s=%s" (Uri.pct_encode ~component:`Userinfo k) (Uri.pct_encode ~component:`Userinfo v))
    |> List.sort ~cmp:String.compare
    |> String.concat ~sep:"&"
  in


  let canonical_request = sprintf "%s\n%s\n%s\n%s\n%s\n%s"
      verb
      (Util.encode_string path)
      canonical_query
      (Buffer.contents canonical_headers)
      signed_headers
      payload_sha
  in
  log "Canonical request:\n%s\n" canonical_request;
  (* This could be cached. Its more or less static *)
  let string_to_sign = sprintf "AWS4-HMAC-SHA256\n%sT%sZ\n%s\n%s"
      date time
      scope
      (hash_sha256 canonical_request |> to_hex)
  in
  log "String to sign:\n%s\n" string_to_sign;
  log "Signed headers:\n%s\n" signed_headers;

  (string_to_sign, signed_headers)

let make_signature ~date ~time ~verb ~path
    ~headers ~query ~scope ~(signing_key:Digestif.SHA256.t) ~payload_sha =
  let (string_to_sign, signed_headers) =
    string_to_sign ~date ~time ~verb ~path ~query ~headers ~payload_sha ~scope
  in
  (hmac_sha256 ~key:(to_raw signing_key) string_to_sign |> to_hex, signed_headers)

let make_auth_header ~credentials ~scope ~signed_headers ~signature =
  sprintf "AWS4-HMAC-SHA256 Credential=%s/%s,SignedHeaders=%s,Signature=%s"
    credentials.Credentials.access_key
    scope
    signed_headers
    signature

let make_presigned_url ?(scheme=`Https) ?host ?port ?(query=[]) ~credentials ~date ~region ~path ~bucket ~verb ~duration () =
  let service = "s3" in
  let ((y, m, d), ((h, mi, s), _)) = Ptime.to_date_time date in
  let verb = match verb with
    | `Get -> "GET"
    | `Put -> "PUT" in
  let scheme = match scheme with
    | `Http -> "http"
    | `Https -> "https" in
  let date = sprintf "%02d%02d%02d" y m d in
  let time = sprintf "%02d%02d%02d" h mi s in
  let (host, path) =
    match host with
    | None -> (sprintf "%s.s3.amazonaws.com" bucket, path)
    | Some h -> (h, sprintf "/%s/%s" bucket path)
  in
  let host_header = match port with
    | None -> host
    | Some p -> String.concat ~sep:":" [host; string_of_int p]
  in
  let duration = string_of_int duration in
  let region = Region.to_string region in
  let headers = Headers.singleton "Host" host_header in
  let query = 
    let base = [
        ("X-Amz-Algorithm", "AWS4-HMAC-SHA256");
        ("X-Amz-Credential", sprintf "%s/%s/%s/s3/aws4_request" credentials.Credentials.access_key date region);
        ("X-Amz-Date", sprintf "%sT%sZ" date time);
        ("X-Amz-Expires", duration);
        ("X-Amz-SignedHeaders", "host");
      ]
    in
    let base_with_token = match credentials.token with
      | None -> base
      | Some token -> ("X-Amz-Security-Token", token) :: base
    in
    (* Merge custom query parameters with AWS parameters *)
    query @ base_with_token
  in
  let scope = make_scope ~date ~region ~service in
  let signing_key = make_signing_key ~date ~region ~service ~credentials () in
  let signature, _signed_headers =
    make_signature ~date ~time ~verb ~path ~headers ~query ~signing_key ~scope ~payload_sha:"UNSIGNED-PAYLOAD"
  in
  let query =
    ("X-Amz-Signature", signature) :: query
    |> List.map ~f:(fun (k, v) -> (k, [v]))
  in
  Uri.make ~scheme ~host ?port ~path ~query ()

let empty_sha_hex = hash_sha256 "" |> to_hex
let chunk_signature ~(signing_key: Digestif.SHA256.t)  ~date ~time ~scope ~previous_signature ~sha =
  let _initial = "STREAMING-AWS4-HMAC-SHA256-PAYLOAD" in
  let string_to_sign = sprintf "AWS4-HMAC-SHA256-PAYLOAD\n%sT%sZ\n%s\n%s\n%s\n%s"
      date time
      scope
      previous_signature
      empty_sha_hex
      (to_hex sha)
  in
  hmac_sha256 ~key:(to_raw signing_key) string_to_sign
