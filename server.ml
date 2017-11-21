open data

type info = string | string list | int

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
  success: bool;
  info: info;
}

let input_of_string s =
  failwith "unimplemented"

let string_of_response res =
  failwith "unimplemented"

let get_users st cmd =
  failwith "unimplemented"

(* [receive] receives message from the server connection, invokes parse to get
 * the command, and finally invokes another function based on command.cmd *)
let receive =
  failwith "unimplemented"

(* [join_chat st cmd] adds userid to user_list in [st], sends the last 10
 * messages of the chat that was joined in the response message. Returns the
 * updated state *)
let join_chat st cmd =
  failwith "unimplemented"

(* [leave_chat st cmd] removes the userid from [st] and sends a response based
 * on the success or failure of the removal. If the chat is not mapped to any
 * userid in the updated state, then the chat is removed from the pub_chat_list
 * or priv_chat_list depending on the type of chat. Sends a response to the
 * client and returns the updated state.
*)
let leave_chat st cmd =
  failwith "unimplemented"

(* [create_user st cmd] initializes the username with a userid and adds the new
 * userid to user_list in [st]. Sends a response to the client and returns the
 * updated state. *)
let create_user st s =
  failwith "unimplemented"

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
let add_msg st cmd =
  failwith "unimplemented"

(* [get_history st cmd] gets the last 10 messages from the chat requested for in
 * [cmd], and sends a response to the client with the chat history. Returns the
 * same state. *)
let get_history st cmd =
  failwith "unimplemented"

(* [create_private_chat st cmd] initializes a chatid for the chat and adds it
 * to the priv_chat_list in [st]. Sends a response to the client and returns the
 * updated state. *)
let create_private_chat st cmd =
  failwith "unimplemented"

(* [create_pub_chat st cmd] initializes a chatid for the chat and adds it
 * to the pub_chat_list in [st]. Sends a response to the client and returns the
 * updated state. *)
let create_pub_chat st cmd =
  failwith "unimplemented"

(* [get_public_chat st cmd] gets the pub_chat_list from [st] and returns the
 * list in the response to the client. Returns the same state. *)
let get_public_chat st cmd =
  failwith "unimplemented"

(* [parse str] passes the string [str] that was received from server and returns
 * stringified response to client *)
let parse st str =
  let input = input_of_string str in
  match input.cmd with
  | Create_user s -> create_user s
  | Send_msg tup -> add_msg
  | Get_public_chats of unit
  | Get_online_users of unit
  | Get_curr_chats of unit
  | Join_chat of string
  | Change_chat of string
  | Get_history of unit
  | Create_private_chat of string
  | Create_group_chat of string
  | Leave_chat of string

  | ADD_MSG -> add_msg st input
  | GET_HISTORY -> get_history st input
  | GET_USERS -> get_users st input
  | CREATE_PRIV_CHAT -> create_private_chat st input
  | CREATE_PUB_CHAT -> create_pub_chat st input
  | CREATE_USER -> create_user st input
  | JOIN_CHAT -> join_chat st input
  | LEAVE_CHAT -> leave_chat st input
  | GET_PUB_CHAT -> get_public_chat st input

let main =
  failwith "unimplemented"
