open Core
open Async
open Client

let st = ref (init_state ())

let print () =
  if !st.print <> []
  then List.iter !st.print (fun x -> print_string (x ^ "\n")); ()

let rec read r =
  Reader.read_line r >>= function
  | `Eof -> (printf "Server error, please try again. \n"; exit 0;)
  | `Ok line ->
    st := parse_receive line !st;
    print ();
    read r

let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; return ())
  | `Ok line ->
    Writer.write_line w (parse_send line !st); send_msg w

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
  | `Ok line -> (handle_create_user line r w)

(* parse string of server response; on success, update state accordingly with
 * username; on failure, print error and loop create_user *)
and handle_create_user res r w =
  st := parse_receive res !st;
  print ();
  if !st.userid = -1 then (print_string "> "; create_user r w) else return ()

let rw_loop r w =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  ()

let chat _ r w =
  create_user r w >>= fun () ->
  print_string "Welcome to the lobby!\n";
  rw_loop r w;
  Deferred.never ()

let rec read_file ~input ~output addr r w file =
  Reader.read_line file >>= function
  | `Eof -> printf "Error reading server\n"; return ()
  | `Ok line -> (print_string line; read_file ~input ~output addr r w file)

let open_file ~input ~output addr r w =
  Reader.open_file input >>= read_file ~input ~output addr r w

let run ~host ~port ~input ~output : unit Async_extra.Import.Deferred.t =
  let addr = Tcp.to_host_and_port host port in
  Tcp.with_connection addr (open_file ~input ~output);
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
      +> flag "-input" (optional_with_default "" string)
        ~doc:" File with test inputs"
      +> flag "-output" (optional_with_default "" string)
        ~doc:" File to output server responses to"
    )
    (fun host port infile outfile -> run ~host ~port ~input ~output)
  |> Command.run

let () = main ()
