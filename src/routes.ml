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

let dynamic_hello_handler req _body =
  let name = Uri.get_query_param (Cohttp.Request.uri req) "name" in
  let response = match name with
    | Some n -> Printf.sprintf "Hello, %s!" n
    | None -> "Hello, stranger!" in
  Server.respond_string ~status:`OK ~body:response ~headers:(Header.init ()) ()

type route = {
  method_: string;
  path: string;
  handler: (Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t);
}

let routes : route list = [
  { method_ = "GET"; path = "/hello"; handler = hello_handler };
  { method_ = "GET"; path = "/goodbye"; handler = goodbye_handler };
  { method_ = "GET"; path = "/hello/name"; handler = hello_name_handler };
  { method_ = "GET"; path = "/hello/:name"; handler = dynamic_hello_handler };
]

let match_route method_ path =
  let rec aux routes =
    match routes with
    | [] -> None
    | { method_; path = route_path; handler } :: rest ->
      if method_ = method_ && (route_path = path || String.contains route_path ':') then
        Some { method_; path = route_path; handler }
      else
        aux rest
  in
  aux routes

let find_route method_ path =
  match match_route method_ path with
  | Some route -> Some route
  | None -> None
