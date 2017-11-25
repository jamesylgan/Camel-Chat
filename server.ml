open Data
open State
open Core
open Async

type info = Nil | String of string | ISList of (int*string) list |
            Int of int | SList of string list

let st = ref (init_state ())
let uid = ref (-1)
let next_uid = fun () -> uid := (!uid) + 1; !uid
let prev_uid = fun () -> uid := (!uid) - 1; !uid
let chatid = ref (-1)
let next_chatid = fun () -> chatid := (!chatid) + 1; !chatid
let prev_chatid = fun () -> chatid := (!chatid) - 1; !chatid

type command =
  | Create_user of string (* username *)
  | Send_msg of (int*string) (* chatid, msg*)
  | Get_public_chats
  | Get_online_users
  | Get_curr_chats
  | Join_chat of string (* chatname*)
  | Get_history of int (* chatid *)
  | Create_priv_chat of string (* username of other user *)
  | Create_pub_chat of string (* chatname *)
  | Leave_chat of string (* chatname *)

type client_input = {
  userid: int;
  cmd: command;
}

type response = {
  userid: int;
  cmd: string;
  success: bool;
  info: info;
}

let input_of_string s =
  {userid = -1; cmd = Create_user s}

let string_of_response res =
  failwith "unimplemented"

let get_users uid =
  try let users = get_online_users !st in
    {userid = uid; cmd = "c"; success = true; info = SList users}
  with UpdateError err ->
    {userid = uid; cmd = "c"; success = false; info = String err}

(* [join_chat st cmd] adds userid to user_list in [st], sends the last 10
 * messages of the chat that was joined in the response message. Returns the
 * updated state *)
let join_chat uid chatname =
  let chatid = get_chatid !st chatname in
  try st := add_user_to_pub_chat !st uid chatid;
    {userid = uid; cmd = "g"; success = true; info = Nil}
  with UpdateError err ->
    {userid = uid; cmd = "g"; success = false; info = String err}

(* [leave_chat st cmd] removes the userid from [st] and sends a response based
 * on the success or failure of the removal. If the chat is not mapped to any
 * userid in the updated state, then the chat is removed from the pub_chat_list
 * or priv_chat_list depending on the type of chat. Sends a response to the
 * client and returns the updated state.
*)
let leave_chat uid chatname =
  let chatid = get_chatid !st chatname in
  try st := remove_from_chat !st uid chatid;
    {userid = uid; cmd = "h"; success = true; info = Nil}
  with UpdateError err ->
    {userid = uid; cmd = "h"; success = false; info = String err}

let create_user username =
  let new_uid = next_uid () in
  try st := add_user !st new_uid username;
    {userid = new_uid; cmd = "f"; success = true; info = Nil}
  with UpdateError err ->
    let _ = prev_uid () in
    {userid = -1; cmd = "f"; success = false; info = String err}

(* [remove_user st cmd] removes the user from user_list in [st]. Sends a
 * response to the client and returns the updated state. *)
let remove_user st cmd =
  failwith "unimplemented"

(* [send rsp] sends the response to the server through the connection. *)
let send rsp =
  failwith "unimplemented"

(* [handle_disconnect st uid] handles if a client of [uid] disconnects from the
 * server. It removes the disconnected [uid] from user_list and from all
 * chats that the user is in. Broadcasts message to all chats that the user was
 * in: "user _ has left". Sends a response to the client and returns the updated
 * state. *)
let handle_disconnect st uid =
  failwith "unimplemented"

(* [broadcast_to_chat st uid] is a helper for [handle_disconnect] to send
 * messages to all chats that the disconnected [uid] was in. *)
let broadcast_to_chat st uid =
  failwith "unimplemented"

(* [get_username st int] is the username associated with [uid] *)
let get_username st uid =
  failwith "unimplemented"

(* [add_msg st cmd] adds a message to chat_msg in [st]. Sends a response to the
 * client and returns the updated state. *)
let send_msg uid tuple =
  try st := add_msg !st uid tuple;
    {userid = uid; cmd = "a"; success = true; info = Nil}
  with UpdateError err ->
    {userid = uid; cmd = "a"; success = false; info = String err}

(* [get_history st cmd] gets the last 10 messages from the chat requested for in
 * [cmd], and sends a response to the client with the chat history. Returns the
 * same state. *)
let get_history uid chatid =
  try let history = get_history !st chatid in
    {userid = uid; cmd = "b"; success = true; info = ISList history}
  with UpdateError err ->
    {userid = uid; cmd = "b"; success = false; info = String err}

(* [create_private_chat st cmd] initializes a chatid for the chat and adds it
 * to the priv_chat_list in [st]. Sends a response to the client and returns the
 * updated state. *)
let create_private_chat uid username =
  let uid2 = get_username !st username in
  let new_chatid = next_chatid () in
  try st := add_priv_chat !st uid uid2 new_chatid;
    {userid = uid; cmd = "d"; success = true; info = Int new_chatid}
  with UpdateError err ->
    let _ = prev_chatid () in
    {userid = uid; cmd = "d"; success = false; info = String err}

(* [create_pub_chat st cmd] initializes a chatid for the chat and adds it
 * to the pub_chat_list in [st]. Sends a response to the client and returns the
 * updated state. *)
let create_pub_chat uid chatname =
  let new_chatid = next_chatid () in
  try st := add_pub_chat !st uid new_chatid chatname;
    {userid = uid; cmd = "e"; success = true; info = Int new_chatid}
  with UpdateError err ->
    let _ = prev_chatid () in
    {userid = uid; cmd = "e"; success = false; info = String err}

(* [get_public_chat st cmd] gets the pub_chat_list from [st] and returns the
 * list in the response to the client. Returns the same state. *)
let get_public_chat uid =
  try let pub_chats = get_pub_chats !st in
    {userid = uid; cmd = "i"; success = true; info = SList pub_chats}
  with UpdateError err ->
  {userid = uid; cmd = "i"; success = false; info = String err}

(* [parse str] passes the string [str] that was received from server and returns
 * stringified response to client *)
let parse str =
  let input = input_of_string str in
  let res = match input.cmd with
    | Create_user s -> create_user s
    | Send_msg tup -> send_msg input.userid tup
    | Get_public_chats -> get_public_chat input.userid
    | Get_online_users -> get_users input.userid
    | Join_chat s -> join_chat input.userid s
    | Get_history chatid -> get_history input.userid chatid
    | Create_priv_chat username -> create_private_chat input.userid username
    | Create_pub_chat chatname -> create_pub_chat input.userid chatname
    | Leave_chat chatname -> leave_chat input.userid chatname
    | Get_curr_chats -> failwith "unim" in
  string_of_response res

let handle_connection _addr r w =
  let () = print_string ("New client \n") in
  Pipe.transfer (Reader.pipe r) (Writer.pipe w)
    (fun x -> parse x)

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
