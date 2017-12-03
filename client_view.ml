open Async
open Client

let st = ref (init_state ())

let b = "\027[0m"
let red = "\027[31m"
let purp = "\027[35m"

let printc_string c s = print_string (c ^ s ^ b)
let printc_endline c s = print_endline (c ^ s ^ b)

let print () =
  let to_print = get_print !st in
  if to_print <> []
  then List.iter (fun x -> print_string (x ^ b ^ "\n")) to_print; ()

let rec read r =
  Reader.read_line r >>= function
  | `Eof -> (printf "Server error, please try again. \n"; exit 0;)
  | `Ok line ->
    st := parse_receive line !st;
    (*print_endline line;*)
    print ();
    read r

let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; return ())
  | `Ok line -> handle_stdin (line |> String.trim |> String.lowercase_ascii) w

and handle_stdin res w =
  match res with
  | "#currchat" -> printc_endline purp (get_curr_chat !st); send_msg w
  | "#mychats" ->
    let chats = get_chats !st in
    printc_endline purp (String.concat ", " chats); send_msg w
  | "#quit" -> exit 0
  | "#help" -> printc_string red ("help message here\n"); send_msg w
  | res ->
    let change_chat = Str.regexp "#goto \\(.+\\)" in
    if Str.string_match change_chat res 0 then (handle_change_chat res w; send_msg w)
    else (Writer.write_line w (parse_send res !st); send_msg w)

and handle_change_chat s w =
  let open String in
  let start = index s ' ' in
  let length = length s in
  let chatname = sub s (start + 1) (length - start - 1) in
  st := change_chat chatname !st;
  print ();
  if (get_print !st <> [])
  then Writer.write_line w (parse_send "#history" !st)

let rec create_user r w =
  let stdin = Lazy.force Reader.stdin in
  let read_std line =
    let is_some =
      begin match String.index_opt line ' ' with
        | Some x -> true
        | None -> false
      end in
    if is_some
    then
      (printf "Error invalid characters in username\n"; print_string "> ";
       create_user r w)
    else if String.length line = 0
    then
      (printf "Error empty username input\n"; print_string "> ";
       create_user r w)
    else (Writer.write_line w (parse_create_user line);
          read_create_username r w) in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; create_user r w)
  | `Ok line -> read_std line

and read_create_username r w =
  Reader.read_line r >>= function
  | `Eof -> (printf "Error reading server\n"; create_user r w)
  | `Ok line -> (handle_create_user line r w)

(* parse string of server response; on success, update state accordingly with
 * username; on failure, print error and loop create_user *)
and handle_create_user res r w =
  st := parse_receive res !st;
  print ();
  if (get_userid !st) = -1 then (printc_string red "> "; create_user r w) else return ()

let rw_loop r w =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  ()

let chat _ r w =
  create_user r w >>= fun () ->
  print_string (red ^ "Welcome to the " ^ purp ^ "lobby" ^ red^ "!\n" ^ b);
  rw_loop r w;
  Deferred.never ()

let run ~host ~port =
  let addr = Tcp.to_host_and_port host port in
  ignore(Tcp.with_connection addr chat);
  Deferred.never ()

let main () =
  printc_string red "Starting Caml Chat... \n";
  printc_string red "Enter a username to begin: \n";
  printc_string red "> ";
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
