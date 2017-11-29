open Core
open Async
open Unix

(* [handle_stdin str] forms the stringified client_input that the client sends
 * to the sever from the command line input [str] *)
let handle_stdin str =
  str ^ "do something"

(* [handle_resp str] takes [str] which is the stringified response from the
 * server, parses it to update its state or prints if necessary. *)
let handle_resp str =
  print_endline (str ^"change from server"); ()

let chat _ r w =
  let stdin = Lazy.force Reader.stdin in
  let rec loop r w =
    printf "> ";
    (* Step one: read a line from the user. *)
    Reader.read_line stdin >>= function
    | `Eof -> (printf "Error reading stdin\n"; return ())
    | `Ok line ->
      (* Step two: send it to the server. *)
      Writer.write_line w (handle_stdin line);
      (* Step three: read back the echoed string. *)
      read r;

and read r =
  Reader.read_line r >>= function
    (* Step four: print it out. *)
  | `Eof -> (printf "Error reading server\n"; return ())
  | `Ok line -> (handle_resp line; loop r w)
in loop r w

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
