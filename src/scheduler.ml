open Lwt.Infix
open Lwt_unix

type task = {
  name: string;
  interval: float;  
  action: unit -> unit Lwt.t; 
}

let tasks = ref []

let add_task name interval action =
  let task = { name; interval; action } in
  tasks := task :: !tasks

let rec run_task task =
  let%lwt () = task.action () in
  let%lwt () = Lwt_unix.sleep task.interval in
  run_task task

let start_scheduled_tasks () =
  Lwt_list.iter_p (fun task ->
    Lwt.async (fun () -> run_task task)
  ) !tasks 