open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

let hello_handler _req _body =
  let response = "Hello, World!" in
  Server.respond_string ~status:`OK ~body:response ~headers:(Header.init ()) ()

let goodbye_handler _req _body =
  let response = "Goodbye, World!" in
  Server.respond_string ~status:`OK ~body:response ~headers:(Header.init ()) ()

let hello_name_handler req _body =
  match Uri.get_query_param (Cohttp.Request.uri req) "name" with
  | Some name -> Server.respond_string ~status:`OK ~body:("Hello, " ^ name) ()
  | None -> Server.respond_string ~status:`Bad_request ~body:"Missing name parameter" ()

let routes =
  [ ("GET", "/hello", hello_handler)
  ; ("GET", "/goodbye", goodbye_handler)
  ; ("GET", "/hello/name", hello_name_handler)
  ] 