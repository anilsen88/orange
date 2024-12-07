open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Utils
open Routes
open Scheduler

type middleware = (Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) -> (Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)

let log_middleware next req body =
  let method_ = Cohttp.Request.meth req in
  let path = Cohttp.Request.uri req |> Uri.path in
  log (Printf.sprintf "Received %s request for %s" (Cohttp.Code.string_of_method method_) path);
  next req body

let cors_middleware next req body =
  let response = next req body in
  Lwt.map (fun (resp, body) ->
    let headers = Cohttp.Response.headers resp in
    let headers = Header.add headers "Access-Control-Allow-Origin" "*" in
    let headers = Header.add headers "Access-Control-Allow-Methods" "GET, POST, OPTIONS" in
    let headers = Header.add headers "Access-Control-Allow-Headers" "Content-Type" in
    (Cohttp.Response.make ~headers () , body)
  ) response

let body_parser_middleware next req body =
  let%lwt body_content = Cohttp_lwt.Body.to_string body in
  let new_req = Cohttp.Request.make ~body:(Cohttp_lwt.Body.of_string body_content) req in
  next new_req (Cohttp_lwt.Body.of_string body_content)

let apply_middlewares middlewares handler req body =
  List.fold_right (fun mw acc -> mw acc) middlewares handler req body

let callback req body =
  let middlewares = [log_middleware; cors_middleware; body_parser_middleware] in
  let method_ = Cohttp.Code.string_of_method (Cohttp.Request.meth req) in
  let path = Uri.path (Cohttp.Request.uri req) in
  match find_route method_ path with
  | Some { handler } ->
    if method_ = "GET" && path = "/ws" then
      Cohttp_lwt.Websocket.accept req >>= fun conn ->
      handler conn req
    else
      apply_middlewares middlewares (fun req body -> handler req body) req body
  | None -> Server.respond_string ~status:`Not_found ~body:"Not Found" ~headers:(Header.init ()) ()

let start_server port =
  let server = Server.make ~callback () in
  let addr = Unix.inet_addr_loopback in
  let uri = Printf.sprintf "http://%s:%d" (Unix.string_of_inet_addr addr) port in
  Printf.printf "Server running at %s\n" uri;
  Server.create ~mode:(`TCP (`Port port)) server

let () =
  let port = 8080 in
  let _ = Lwt_main.run (start_scheduled_tasks () >>= fun () -> start_server port) in
  () 