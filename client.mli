type info

type command

(* A type representing the user's id and the current chat's id. *)
type state = {
  userid : int;
  curr_chatid : int;
  chats : int list
}

(* A type representing the information the server sends back in response to
 * the client's command. *)
type server_response = {
  success : bool;
  info : string
}

(* A type representing the information that gets sent to the server based on
 * a user's command.
 * - [userid] contains the id of the sending user.
 * - [cmd] contains what command the user issues.
 * - [info] contains the specific information associated with the command.
 *   For example, if the user calls [send_msg], [info] contains the message.
*)
type client_output = {
  userid : int;
  cmd: command;
  info: info
}

(* [main ()] is the main function that starts the REPL by reading user input,
 * sending data to the server, and handling the server's response. *)
val main : unit -> unit

(* [parse s] is the command that gets parsed from a user's input string
 * [s]. Raises "Invalid command" if [s] is not a command. *)
val parse : string -> command

(* [create_user s] is the client output that will get sent to the server when
 * the user initiates his or her username as [s]. *)
val create_user : string -> client_output

(* [send_msg s] is the client output that will get sent to the server when
 * the user sends a message [s]. *)
val send_msg : string -> client_output

(* [get_pulic_chats] is the client output that will get sent to the server
 * when the user requests the list of public chats available to join in.
*)
val get_public_chats : unit -> client_output

(* [get_online_users] is the client output that will get sent to the server
 * when the user requests the list of current online users. *)
val get_online_users : unit -> client_output

(* [get_curr_chats] is the client output that will get sent to the server
 * when the user requests the list of current chats, both private AND
 * public. *)
val get_curr_chats : unit -> client_output

(* [join_chat s] returns the client output that will get sent to the server
 * when the user requests to join the chat by name [s]. *)
val join_chat : string -> client_output

(* [change_chat s] returns the client output that will get sent to the server
 * when the user requests to switch to a different chat by the name [s]. *)
val change_chat : string -> client_output

(* [help] is the string will get printed when the user requests for help using
 * the chat server *)
val help : unit -> string

(* [get_history] is the client output that will be sent to the server when
 * the user requests for past chatting messages up to a limit of ten. *)
val get_history : unit -> client_output

(* [create_private_chat s] is the client output that will be sent to the server
 * when the user requests to initiate a private chat with another user of
 * username [s]. *)
val create_private_chat : string -> client_output

(* [create_group_chat s] is the client output that will be sent to the server
 * when the user requests to initiate a group chat of chat name [s]. *)
val create_group_chat : string -> client_output

(* [leave_chat s] is the client output that will be sent to the server when
 * the user requests to leave a chat named [s]. *)
val leave_chat : string -> client_output

(* [send c] sends [c] of type [client_output] to the server. *)
val send : client_output -> unit

(* [receive] receives data from the server and returns the [server_response]
 * containing that data. *)
val receive : unit -> server_response
