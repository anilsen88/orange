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

let user_handler req _body params =
  match params with
  | [id] -> Server.respond_string ~status:`OK ~body:("User ID: " ^ id) ~headers:(Header.init ()) ()
  | _ -> Server.respond_string ~status:`Bad_request ~body:"Invalid parameters" ()

type route = {
  method_: string;
  path: string;
  version: string;
  handler: (Cohttp.Request.t -> Cohttp_lwt.Body.t -> (string list -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t));
}

let clients = ref []

let broadcast_message message =
  Lwt_list.iter_p (fun (ws, _) ->
    Cohttp_lwt.Websocket.send ws message
  ) !clients

let websocket_handler (conn : Cohttp_lwt.Websocket.t) req =
  let client_id = Printf.sprintf "%s" (Uri.to_string (Cohttp.Request.uri req)) in
  clients := (conn, client_id) :: !clients;  (* Add new client to the list *)

  let rec listen () =
    Cohttp_lwt.Websocket.recv conn >>= function
    | Ok message ->
      let broadcast_message = Printf.sprintf "Client %s says: %s" client_id message in
      broadcast_message broadcast_message >>= fun () ;
      listen ()  (* Continue listening for messages *)
    | Error _ ->
      clients := List.filter (fun (c, _) -> c <> conn) !clients;  (* Remove client on error *)
      Lwt.return ()

  in
  listen ()

let websocket_route = { method_ = "GET"; path = "/ws"; version = "v1"; handler = websocket_handler }

let routes : route list = [
  { method_ = "GET"; path = "/v1/hello"; version = "v1"; handler = hello_handler };
  { method_ = "GET"; path = "/v2/hello"; version = "v2"; handler = hello_handler };
  { method_ = "GET"; path = "/v1/goodbye"; version = "v1"; handler = goodbye_handler };
  { method_ = "GET"; path = "/v2/goodbye"; version = "v2"; handler = goodbye_handler };
  { method_ = "GET"; path = "/v1/hello/name"; version = "v1"; handler = hello_name_handler };
  { method_ = "GET"; path = "/v2/hello/name"; version = "v2"; handler = hello_name_handler };
  { method_ = "GET"; path = "/v1/users/:id"; version = "v1"; handler = user_handler };
  { method_ = "GET"; path = "/v2/users/:id"; version = "v2"; handler = user_handler };
  websocket_route;
]

let match_route method_ path =
  let rec aux routes =
    match routes with
    | [] -> None
    | { method_; path = route_path; version; handler } :: rest ->
      let path_parts = String.split_on_char '/' path in
      let route_parts = String.split_on_char '/' route_path in
      if method_ = method_ && List.length path_parts = List.length route_parts then
        let params = List.fold_left2 (fun acc part route_part ->
          match route_part with
          | p when String.contains p ':' -> 
              let param_name = String.sub p 1 (String.length p - 1) in
              param_name :: acc
          | _ when part = route_part -> acc
          | _ -> raise Exit
        ) [] path_parts route_parts in
        Some { method_; path = route_path; version; handler = (fun req body -> handler req body params) }
      else
        aux rest
  in
  try aux routes with Exit -> None

let find_route method_ path =
  match match_route method_ path with
  | Some route -> Some route
  | None -> None
