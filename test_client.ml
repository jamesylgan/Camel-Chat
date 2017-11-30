open Core
open Async
open Unix

(* [handle_stdin str] forms the stringified client_input that the client sends
 * to the sever from the command line input [str] *)
let handle_stdin str =
  str

(* [handle_resp str] takes [str] which is the stringified response from the
 * server, parses it to update its state or prints if necessary. *)
let handle_resp str =
  print_endline (str); ()

let rec read r =
  Reader.read_line r >>= function
  | `Eof -> (printf "Server error, please try again. \n"; exit 0;)
  | `Ok line -> (handle_resp line; read r)

let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; return ())
  | `Ok line ->
    Writer.write_line w (handle_stdin line); send_msg w

let chat _ r w =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  Deferred.never ()

let run ~host ~port =
  let addr = Tcp.to_host_and_port host port in
  ignore(Tcp.with_connection addr chat);
  Deferred.never ()

let main () =
  print_string "Starting Caml Chat... \n";
  print_string "Enter \"#quit\" to exit. \n";
  Command.async
    ~summary:"Start the chat client"
    Command.Spec.(
      empty
      +> flag "-host" (optional_with_default "127.0.0.1" string)
        ~doc:" Port to listen on (default 9999)"
      +> flag "-port" (optional_with_default 9999 int)
        ~doc:" Port to listen on (default 9999)"
    )
    (fun host port () -> run ~host ~port)
  |> Command.run

let () = main ()
