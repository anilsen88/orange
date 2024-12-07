open OUnit2
open Cohttp
open Cohttp_lwt_unix

let test_hello_handler _ =
  let uri = Uri.of_string "http://localhost:8080/hello" in
  let%lwt response, body = Client.get uri in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  assert_equal response.status `OK;
  assert_equal body "Hello, World!";
  Lwt.return ()

let test_goodbye_handler _ =
  let uri = Uri.of_string "http://localhost:8080/goodbye" in
  let%lwt response, body = Client.get uri in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  assert_equal response.status `OK;
  assert_equal body "Goodbye, World!";
  Lwt.return ()

let suite =
  "Test Routes" >::: [
    "test_hello_handler" >:: test_hello_handler;
    "test_goodbye_handler" >:: test_goodbye_handler;
  ]

let () =
  run_test_tt_main suite 