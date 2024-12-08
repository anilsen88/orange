open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic

let validate_json_body body =
  let%lwt body_string = Cohttp_lwt.Body.to_string body in
  try
    let _ = Yojson.Basic.from_string body_string in
    Lwt.return (Ok ())
  with Yojson.Json_error msg ->
    Lwt.return (Error ("Invalid JSON: " ^ msg))

let validate_query_params req expected_params =
  let actual_params = Uri.query (Request.uri req) in
  let missing_params = List.filter (fun param -> not (List.mem_assoc param actual_params)) expected_params in
  if List.length missing_params = 0 then
    Lwt.return (Ok ())
  else
    Lwt.return (Error ("Missing query parameters: " ^ String.concat ", " missing_params)) 