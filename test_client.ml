open Core
open Async
open Client

let st = ref (init_state ())

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

let rec create_user r w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; create_user r w)
  | `Ok line ->
    Writer.write_line w (parse_create_user line);
    read_create_username r w

and read_create_username r w =
  Reader.read_line r >>= function
  | `Eof -> (printf "Error reading server\n"; create_user r w)
  | `Ok line -> return (handle_create_user line r w)

(* parse string of server response; on success, update state accordingly with
 * username; on failure, print error and loop create_user *)
and handle_create_user res r w =
  print_string res;
  st := parse_receive res !st;
  List.iter !st.print (fun str -> print_string str);
(*if !st.userid = -1 then create_user r w else return ()*)
  return ()

let rw_loop r w _ =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  ()

let chat _ r w =
  create_user r w >>= fun x ->
  rw_loop r w x;
  Deferred.never ()

let run ~host ~port =
  let addr = Tcp.to_host_and_port host port in
  ignore(Tcp.with_connection addr chat);
  Deferred.never ()

let main () =
  print_string "Starting Caml Chat... \n";
  print_string "Enter a username to begin: \n";
  print_string "> ";
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
