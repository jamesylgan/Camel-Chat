open data

(* typically a string or string list depending on implementation *)
type info

(* [cmd] is the type of commands that can be called from the client *)
type cmd = ADD_MSG | GET_HISTORY | GET_USERS | CREATE_PRIV_CHAT |
           CREATE_PUB_CHAT | CREATE_USER | JOIN_CHAT |
           LEAVE_CHAT

(* [client_input] is the message that the server receives from the client *)
type client_input = {
  userid: int;
  cmd: cmd;
  info: info
}

(* [response] is the message that the server sends to the client *)
type response = {
  userid: int;
  success: bool;
  info: info
}

(* [state] is the current state of the server. The server maintains a 4
 * dictionaries:
   - user_list maps userid to usernames
   - priv_chat_list maps each private chatid to a list of userids
   - pub_chat_list maps each public chatid to a list of userids
   - chat_msg maps each chatid to a (int * string) list (ie userid, messages of
     the chat)
   - pub_chat_name maps each public chat to its name (private chats do not have
     a name)
   - chat_msg maps the chatid to a list of messages that was sent in the chat
 *)
type state = {
  user_list: Dictionary;
  priv_chat_list: Dictionary;
  pub_chat_list: Dictionary;
  pub_chat_name: Dictionary;
  chat_msg: Dictionary;
}

(* [input_of_string s] is the client_input record of [s] *)
val input_of_string: string -> client_input

(* [response_of_string s] is the response record of [s] *)
val response_of_string: string -> response

(* [string_of_input input] is the string of [input] *)
val string_of_input: client_input -> string

(* [string_of_response res] is the string of [res] *)
val string_of_response: response -> string

(* [main] is the main function that loops on [receive] and updates the local
 * state *)
val main: unit

(* [parse str] passes the string [str] that was received from server and returns
 * the corresponding command type *)
val parse: string -> command

(* [receive] receives message from the server connection, invokes parse to get
 * the command, and finally invokes another function based on command.cmd *)
val receive: unit -> state

(* [join_chat st cmd] adds userid to user_list in [st], sends the last 10
 * messages of the chat that was joined in the response message. Returns the
 * updated state *)
val join_chat: state -> command -> state

(* [leave_chat st cmd] removes the userid from [st] and sends a response based
 * on the success or failure of the removal. If the chat is not mapped to any
 * userid in the updated state, then the chat is removed from the pub_chat_list
 * or priv_chat_list depending on the type of chat. Sends a response to the
 * client and returns the updated state.
 *)
val leave_chat: state -> command -> state

(* [create_user st cmd] initializes the username with a userid and adds the new
 * userid to user_list in [st]. Sends a response to the client and returns the
 * updated state. *)
val create_user: state -> command -> state

(* [remove_user st cmd] removes the user from user_list in [st]. Sends a
 * response to the client and returns the updated state. *)
val remove_user: state -> command -> state

(* [send rsp] sends the response to the server through the connection. *)
val send: response -> unit

(* [handle_disconnect st uid] handles if a client of [uid] disconnects from the
 * server. It removes the disconnected [uid] from user_list and from all
 * chats that the user is in. Broadcasts message to all chats that the user was
 * in: "user _ has left". Sends a response to the client and returns the updated
 * state. *)
val handle_disconnect: state -> int -> state

(* [broadcast_to_chat st uid] is a helper for [handle_disconnect] to send
 * messages to all chats that the disconnected [uid] was in. *)
val broadcast_to_chat: state -> ()

(* [get_username st int] is the username associated with [uid] *)
val get_username: state -> int -> string

(* [add_msg st cmd] adds a message to chat_msg in [st]. Sends a response to the
 * client and returns the updated state. *)
val add_msg: state -> command -> state

(* [get_history st cmd] gets the last 10 messages from the chat requested for in
 * [cmd], and sends a response to the client with the chat history. Returns the
 * same state. *)
val get_history: state -> command -> state

(* [create_private_chat st cmd] initializes a chatid for the chat and adds it
 * to the priv_chat_list in [st]. Sends a response to the client and returns the
 * updated state. *)
val create_private_chat: state -> command -> state

(* [create_pub_chat st cmd] initializes a chatid for the chat and adds it
 * to the pub_chat_list in [st]. Sends a response to the client and returns the
 * updated state. *)
val create_pub_chat: state -> command -> state

(* [get_public_chat st cmd] gets the pub_chat_list from [st] and returns the
 * list in the response to the client. Returns the same state. *)
val get_public_chat: state -> command -> state
