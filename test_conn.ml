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

let rec read_file input output addr r w reader_line =
  Reader.read_line reader_line >>= function
  | `Eof -> printf "Error reading file\n"; return ()
  | `Ok line -> (print_endline ("once" ^line); read_file input output addr r w reader_line)

let create_user_hardcode username r w =
  Writer.write_line w (parse_create_user username);
  Reader.read_line r >>= function
  | `Eof -> (printf "Error reading server\n"; return ())
  | `Ok line ->
    st := parse_receive line !st;
    print ();
    return ()

let rec handle_stdin res w =
  match res with
  | "#currchat" -> printc_endline purp (get_curr_chat !st); send_msg w
  | "#mychats" ->
    print_string "is my chats\n";
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
  if (get_print !st = [red ^ "Entering chat " ^ purp ^ chatname ^ red ^ "..."])
  then Writer.write_line w (parse_send "#history" !st)

let rec send_cmd r w lst =
  match lst with
  | [] -> return []
  | cmd::t ->
    ignore (handle_stdin (cmd |> String.trim |> String.lowercase_ascii) w);
    Reader.read_line r >>= function
    | `Eof -> print_endline "comes here?"; return []
    | `Ok line ->
      print_endline "never comes here?";
      st := parse_receive line !st;
      print_endline "never comes here?";
      print ();
      print_endline "never comes here?";
      send_cmd r w t

let rec open_file addr r w  =
  Reader.file_contents "test1.txt"
  >>= fun text -> let lst = Core.String.split text ~on:'\n' in
  match lst with
  | [] -> return ()
  | h::t ->
    print_endline h;
    create_user_hardcode h r w
    >>= fun () ->
    print_string (red ^ "Welcome to the " ^ purp ^ "lobby" ^ red^ "!\n" ^ b);
    ignore(send_cmd r w t);
    return ()
(*Reader.open_file "test1.txt" >>= read_file input output addr r w*)

let x = ref 0


let run ~host ~port () : unit Async_extra.Import.Deferred.t =
  (after (Core.sec 5.)) >>= (fun () -> print_endline ("fst"); return ())
  >>= fun () -> (after (Core.sec 3.)) >>= fun () -> x := !x + 1; print_endline ("snd"); return(); Deferred.never ()

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
    (fun host port -> run ~host ~port)
  |> Command.run

let () = main ()
