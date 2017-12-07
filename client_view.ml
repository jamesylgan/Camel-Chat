open Async
open Client

(* [st] is the initial state of the client. *)
let st = ref (init_state ())

let b = "\027[0m"
let red = "\027[31m"
let purp = "\027[35m"
let green = "\027[32m"
let cyan = "\027[36m"
let blue = "\027[34m"
let yellow = "\027[1;33m"

let caml = "                 ,,__
      ..  ..   / o._)   ___   ____                _
     /--'/--\\  \\-'||   / _ \\ / ___|__ _ _ __ ___ | |
    /        \\_/ / |  | | | | |   / _` | '_ ` _ \\| |
  .'\\  \\__\\  __.'.'   | |_| | |__| (_| | | | | | | |
    )\\ |  )\\ |         \\___/ \\____\\__,_|_| |_| |_|_|
   // \\\\ // \\\\
  ||_  \\\\|_  \\\\_    -- two humps are better than one
  '--' '--'' '--'   -- we are groot
source: https://github.com/avsm/vagrant-opam/blob/0ba2974e819390764725a0e18e188f455a14d6ac/bootstrap.sh\n"

let help_message = " Get current chat history: #history
  Create private chat with another user: #chatwith <username>
  Create a public chat: #makechat <chat name>
  Join a new chat: #join <chat name>
  Leave a chat: #leave <chat name>
  See a list of users currently online: #users
  See a list of ongoing public chats: #pubchats
  See a list of chats you are currently in: #mychats
  See which chat you are currently viewing: #currchat
  View a different chat that you are in: #goto <chat name>
  Quit out of Camel Chat: #quit
  ocaml_is_bae: #camelchat
  FAQ for common issues: #faq
  View this message again: #help\n"

let faq_message = " 1. When someone starts a private chat with you,
    you have to #goto [username] to view messages
  2. You can never leave a private chat: make new friends :)
  3. You start in a lobby chat, and you cannot leave the lobby
  4. Your username cannot be the same as a public group name,
    and you cannot make public groups with
    names identical to existing usernames\n"

let printc_string c s = print_string (c ^ s ^ b)
let printc_endline c s = print_endline (c ^ s ^ b)

(* [print ()] prints the messages in st.print *)
let print () =
  let to_print = get_print !st in
  if to_print <> []
  then List.iter (fun x -> print_string (x ^ b ^ "\n")) to_print; ()

(* [read r] performs the read loop of on pipe [r] from the server *)
let rec read r =
  Reader.read_line r >>= function
  | `Eof -> (printf "Server error, please try again. \n"; exit 0;)
  | `Ok line ->
    st := parse_receive line !st;
    print ();
    read r

(* [send_msg w] trims white space from the standard input recursively calls
 * send_msg to send the input to the server. *)
let rec send_msg w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; return ())
  | `Ok line -> handle_stdin (line |> String.trim) w

(* [handle_stdin res w] is the helper function to check if [res] is a local
 * command rather than a message to be sent *)
and handle_stdin res w =
  match res with
  | "#currchat" -> printc_endline purp (get_curr_chat !st); send_msg w
  | "#mychats" ->
    let chats = get_chats !st in
    printc_endline purp (String.concat ", " chats); send_msg w
  | "#quit" -> exit 0
  | "#help" -> printc_string red (help_message); send_msg w
  | "#faq" -> printc_string red (faq_message);
    send_msg w
  | "#camelchat" -> printc_string yellow (caml); send_msg w
  | res ->
    let change_chat = Str.regexp "#goto \\(.+\\)" in
    if Str.string_match change_chat res 0
    then (handle_change_chat res w; send_msg w)
    else
      begin
        st := check_chat res !st;
        if get_print !st = []
        then (Writer.write_line w (parse_send res !st); send_msg w)
        else (print (); send_msg w)
      end

(* [handle_change_chat s w] handles change chat. Prints history if there was no
 * error in the change chat call. *)
and handle_change_chat s w =
  let open String in
  let start = index s ' ' in
  let length = length s in
  let chatname = sub s (start + 1) (length - start - 1) in
  st := change_chat chatname !st;
  print ();
  let error = Str.regexp ("\027\[31mError:\\(.+\\)") in
  if (not (Str.string_match error (List.hd (get_print !st)) 0))
  then Writer.write_line w (parse_send "#history" !st)

(* [create_user r w] is sends the username from standard input to the server.
 * Checks if the username is a valid username. *)
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
      (print_string (red ^ "Error: Invalid characters in username\n> ");
       create_user r w)
    else if String.length line = 0
    then
      (print_string (red ^ "Error: Empty username input\n> ");
       create_user r w)
    else (Writer.write_line w (parse_create_user line);
          read_create_username r w) in
  Reader.read_line stdin >>= function
  | `Eof -> (printf "Error reading stdin\n"; create_user r w)
  | `Ok line -> read_std line

(* [read_create_username r w] reads the response from the server. *)
and read_create_username r w =
  Reader.read_line r >>= function
  | `Eof -> (printf "Error reading server\n"; create_user r w)
  | `Ok line -> (handle_create_user line r w)

(* [handle_create_user] parses the string of server response; on success,
 * update state accordingly with username; on failure, print error and loop
 * create_user *)
and handle_create_user res r w =
  st := parse_receive res !st;
  print ();
  if (get_userid !st) = -1 then (printc_string red "> "; create_user r w) else return ()

(* [rw_loop r w] initializes the read and write asynchronous loops. *)
let rw_loop r w =
  don't_wait_for (send_msg w);
  don't_wait_for (read r);
  ()

(* [chat _ r w] calls create user. When the deferred object is returned from
 * create_user, the function starts the read write loop and never returns. *)
let chat _ r w =
  create_user r w >>= fun () ->
  print_string (red ^ "Welcome to the " ^ purp ^ "Lobby" ^ red^ "!\nType "
                ^ blue ^ "#help" ^ red ^ " for instructions!\n" ^ b);
  rw_loop r w;
  Deferred.never ()

(* [run host port] connects to the server at [host] and [port] and calls the
 * [chat] command. *)
let run ~host ~port =
  let addr = Tcp.to_host_and_port host port in
  ignore(Tcp.with_connection addr chat);
  Deferred.never ()

(* [main ()] is the main function of client server that starts the scheduler
 * and reads from the command line arguments. *)
let main () =
  print_endline (red ^ "S" ^ yellow ^ "t" ^ green ^ "a" ^
                 cyan ^ "r" ^ blue ^ "t" ^ purp ^ "i" ^ red ^ "n" ^ yellow ^ "g"
                 ^ green ^ " C" ^ cyan ^ "a" ^ blue ^ "m" ^ purp ^ "e" ^ red
                 ^ "l" ^ yellow ^ " C" ^ green ^ "h" ^ cyan ^ "a" ^ blue
                 ^ "t" ^ b);
  printc_string yellow (caml);
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
