open Core
open Async

type response = {
  userid: int;
  success: bool;
  info: string
}

exception Quit

let handle_connection _addr r w =
  let () = print_string ("New client \n") in
  Pipe.transfer (Reader.pipe r) (Writer.pipe w)
    (fun x -> let () = print_string ("Received command: " ^ x) in
      if x = "hello\n" then "yes\n" else "no\n")

let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w -> handle_connection _addr r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let quit_regex = Str.regexp {|^#quit\(;;\)?$|}

let matches s r =
  Str.string_match r s 0

let handle_stdin input =
  if matches input quit_regex then raise Quit
  else print_string "Invalid command\n"; ()

let main () =
  print_string "Starting chat server... \n";
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
