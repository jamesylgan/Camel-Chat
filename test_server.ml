open Core
open Async

type response = {
  userid: int;
  success: bool;
  info: string
}

exception Quit

(*I don't completely understand this, but
  what purpose does ref serve? Can we not use ints?
*)
let st = ref []
let uid = ref (-1)
let next_uid = fun () -> uid := (!uid) + 1; !uid

let parse msg curr_uid =
  List.iter !st
    (fun (uid,(r,w)) ->
       if curr_uid <> uid && Writer.is_open w
       then Writer.write w (string_of_int curr_uid ^ " says: " ^ msg));
  let () = print_string ("Received command: " ^ msg) in
  if msg = "hello\n" then "yes\n" else "no\n"

let handle_connection _addr r w =
  let () = print_string ("New client \n") in
  let curr_uid = next_uid () in
  st := (curr_uid,(r,w))::!st;
  Pipe.transfer (Reader.pipe r) (Writer.pipe w)
    (fun x -> parse x curr_uid)

let quit_regex = Str.regexp {|^#quit\(;;\)?$|}

let matches s r =
  Str.string_match r s 0

let handle_stdin input =
  if matches input quit_regex then let _ = exit 0 in ()
  else print_string "Invalid command\n"; ()

let rec read_cmdline () =
  let stdin : Reader.t = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= fun res -> return
    begin
      match res with
      | `Ok str -> handle_stdin str
      | `Eof ->  ()
    end;
  ignore (read_cmdline());
  Deferred.never ()

let create_tcp ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w -> handle_connection _addr r w) in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let run ~port =
  ignore (read_cmdline ());
  ignore (create_tcp port);
  Deferred.never ()

let main () =
  print_string "Starting chat server... \n";
  print_string "Enter \"#quit\" to shutdown the server. \n";
  print_string  "> ";
  Command.async
    ~summary:"Start the chat server"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 9999 int)
        ~doc:" Port to listen on (default 9999)"
    )
    (fun port () -> run ~port)
  |> Command.run

let () = main ()
