open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Utils
open Routes

let callback req body =
  let method_ = Cohttp.Request.meth req in
  let path = Cohttp.Request.uri req |> Uri.path in
  log (Printf.sprintf "Received %s request for %s" (Cohttp.Code.string_of_method method_) path);
  
  match List.find_opt (fun (m, p, _) -> m = Cohttp.Code.string_of_method method_ && p = path) routes with
  | Some (_, _, handler) -> handler req body
  | None -> Server.respond_string ~status:`Not_found ~body:"Not Found" ~headers:(Header.init ()) ()

let start_server port =
  let server = Server.make ~callback () in
  let addr = Unix.inet_addr_loopback in
  let uri = Printf.sprintf "http://%s:%d" (Unix.string_of_inet_addr addr) port in
  Printf.printf "Server running at %s\n" uri;
  Server.create ~mode:(`TCP (`Port port)) server

let () =
  let port = 8080 in
  let _ = Lwt_main.run (start_server port) in
  () 