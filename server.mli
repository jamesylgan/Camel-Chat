open Data
open State

(* TODO: update the specifications.*)

(* typically an int or string or string list depending on implementation *)
type info

(* [command] is the type of commands that can be called from the client *)
type command

(* [client_input] is the message that the server receives from the client *)
type client_input

(* [response] is the message that the server sends to the client *)
type response

(* [input_of_string s] is the client_input record of [s] *)
val input_of_string: string -> client_input

(* [string_of_response res] is the string of [res], where string is the
 * formatted string to send to the client (refer to documentation) *)
val string_of_response: response -> string

(* [main] is the main function that loops on [receive] and updates the local
 * state *)
val main: unit -> unit

(* [parse str] passes the string [str] that was received from server and returns
 * stringified response to client *)
val parse: string -> string

(* [join_chat uid chatname cmd] adds userid to pub_chat_list in [st]. Returns
 * the response of the server. *)
val join_chat: int -> string -> response

(* [leave_chat uid chatname] removes the userid from [st] and sends a response
 * based on the success or failure of the removal. If the chat is not mapped to
 * any userid in the updated state, then the chat is removed from the
 * pub_chat_list or priv_chat_list depending on the type of chat. Sends a
 * response to the client and returns the updated state.
 *)
val leave_chat: int -> string -> response

(* [create_user username] initializes the username with a userid and adds the new
 * userid to user_list in [st]. Sends a response to the client and returns the
 * updated state. *)
val create_user: string -> response

(* [delete_user uid] removes the user from user_list in [st]. Sends a
 * response to the client and returns the updated state. *)
val delete_user: int -> response

(* [handle_disconnect st uid] handles if a client of [uid] disconnects from the
 * server. It removes the disconnected [uid] from user_list and from all
 * chats that the user is in. Broadcasts message to all chats that the user was
 * in: "user _ has left". Sends a response to the client and returns the updated
 * state. *)
val handle_disconnect: state -> int -> state

(* [broadcast_to_chat uid (chatid, msg)] is a helper for [handle_disconnect] to
 * send messages to all chats that the disconnected [uid] was in. *)
val broadcast_to_chat: int -> (int * string) -> unit

(* [get_users uid] is the username of the uid *)
val get_users: int -> response

(* [get_history uid chatid] gets the last 10 messages from chatid, and returns a
 * response to the client with the chat history. *)
val get_history: int -> int -> response

(* [create_private_chat uid username] initializes a chatid for the chat and adds
 * it to the priv_chat_list in [st]. Returns a response with success/failure. *)
val create_private_chat: int -> string -> response

(* [create_pub_chat uid chatname] initializes a chatid for the chat and adds it
 * to the pub_chat_list in [st]. Returns a response with success/failure. *)
val create_pub_chat: int -> string -> response

(* [get_public_chat uid] gets the pub_chat_list from [st] and returns a response
 * with the list in the response to the client. *)
val get_public_chat: int -> response

(* [send_msg uid (chatid, msg)] sends [msg] to all users in the [chatid]. Adds
 * the tuple to the list of st.chat_msg and returns a response with success
 * or failure. *)
val send_msg: int -> (int * string) -> response
