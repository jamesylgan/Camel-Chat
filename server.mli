open State

(* typically an int or string or string list depending on implementation *)
type info = Nil | String of string | ISList of (int * string) list |
            Int of int | SList of string list | SSTuple of (string * string)

(* [command] is the type of commands that can be called from the client *)
type command =
  | Create_user of string (* username *)
  | Send_msg of (int*string) (* chatid, msg*)
  | Get_public_chats
  | Get_online_users
  | Join_chat of string (* chatname*)
  | Get_history of int (* chatid *)
  | Create_priv_chat of string (* username of other user *)
  | Create_pub_chat of string (* chatname *)
  | Leave_chat of string (* chatname *)

(* [client_input] is the message that the server receives from the client *)
type client_input = {
  userid: int;
  cmd: command;
}

(* [response] is the message that the server sends to the client *)
type response = {
  userid: int;
  cmd: string;
  success: bool;
  info: info;
  chatid: int
}

(* [view_state] is the state of the server_view. [uid] is the counter for the
 * uid of the last user added. [chatid] is the chatid of the last chat added.
 * [response] is Some response that is to be passed to the server module from
 * the view. [res_string] is the string of the server_response to pass back to
 * the view. *)
type view_state = {
  state : State.state;
  uid : int;
  chatid : int;
  response: response option;
  res_string : string
}

(* [init_state] is the initial state of the view_state. *)
val init_state: unit -> view_state

(* [input_of_string s] is the client_input record of [s] *)
val input_of_string: string -> client_input

(* [string_of_response res] is the string of [res], where string is the
 * formatted string to send to the client (refer to documentation) *)
val string_of_response: response -> string

(* [parse st str r w] passes the string [str] that was received from server and
 * returns a view_state with the stringified response to client *)
val parse: view_state -> string -> Async.Reader.t -> Async.Writer.t -> view_state

(* [join_chat st uid chatname cmd] adds userid to pub_chat_list in [st.state].
 * Returns the updated view_state of the server. *)
val join_chat: view_state -> int -> string -> view_state

(* [leave_chat st uid chatname] removes the userid from [st].state and sends a
 * response based on the success or failure of the removal. If the chat is not
 * mapped to any userid in the updated state, then the chat is removed from the
 * pub_chat_list or priv_chat_list depending on the type of chat. Sends a
 * response to the client and returns updated view_state of the server.
 *)
val leave_chat: view_state -> int -> string -> view_state

(* [create_user st username r w] initializes the username with a userid and adds
 * the new userid to user_list in [st]. Adds (r,w) to current connections. Sends
 * a response to the client and returns updated view_state of the server. *)
val create_user: view_state -> string -> Async.Reader.t -> Async.Writer.t -> view_state

(* [handle_disconnect st uid] handles if a client of [uid] disconnects from the
 * server. It removes the disconnected [uid] from user_list and from all
 * chats that the user is in. Broadcasts message to all chats that the user was
 * in: "user _ has left". Sends a response to the client. Returns the updated
 * view_state of the server *)
val handle_disconnect: view_state -> int -> view_state

(* [broadcast_to_chat st uid (chatid, msg) msg_or_notif] is a helper for
 * [handle_disconnect] to send messages to all chats that the disconnected [uid]
 * was in. [msg_or_notif] is used to determine if the broadcast is by the server
 * (eg someone left the chat) or a send message request. Returns the updated
 * view_state of the server *)
val broadcast_to_chat: view_state -> int -> int * string ->
  [< `MSG | `NOTIF of string > `NOTIF ] -> view_state

(* [get_users st uid] is the username of the uid. Returns the updated view_state
 * of the server *)
val get_users: view_state -> int -> view_state

(* [get_history st uid chatid] gets the last 10 messages from chatid, and
 * returns a response to the client with the chat history in an updated
 * view_state. *)
val get_history: view_state -> int -> int -> view_state

(* [create_private_chat st uid username] initializes a chatid for the chat and
 * adds it to the priv_chat_list in [st]. Returns a response with
 * success/failure in the updated view_state. *)
val create_private_chat: view_state -> int -> string -> view_state

(* [create_pub_chat st uid chatname] initializes a chatid for the chat and adds
 * it to the pub_chat_list in [st]. Returns a response with success/failure in
 * the updated view_state. *)
val create_pub_chat: view_state -> int -> string -> view_state

(* [get_public_chat st uid] gets the pub_chat_list from [st] and returns a
 * response with the list in the response to the client in the updated
 * view_state. *)
val get_public_chat: view_state -> int -> view_state

(* [send_msg st uid (chatid, msg)] sends [msg] appended with the username of
 * [uid] to all users in the [chatid]. Adds the tuple to the list of st.chat_msg
 * and returns a response with success or failure in the updated view_state. *)
val send_msg: view_state -> int -> (int * string) -> view_state
