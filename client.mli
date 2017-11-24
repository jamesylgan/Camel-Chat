(* A helper type that represents the information sent back from the server,
 * typically a string or string list. *)
type info

(* A type that represents the input associated with a command-specific
 * function. Normally a string, for functions like [send_msg], or unit for
 * functions like [get_history]. *)
type command

(* A type representing the user's id and the current chat's id. *)
type state = {
  userid : int;
  curr_chatid : int;
  chats : int list
}

(* A type representing the information the server sends back in response to
 * the client's command.
 * - [userid] contains the id of the sending user.
 * - [success] contains whether the server successfully handles the [user-input].
 * - [info] contains the specific information associated with the command.
 *   For example, if the user calls [send_msg], [info] contains the message.
*)
type server_response = {
  userid: int;
  success : bool;
  info : info;
}

(* A type representing the information that gets sent to the server based on
 * a user's command.
 * - [userid]: Same as above
 * - [cmd] contains what command the user issues.
*)
type client_output = {
  userid : int;
  cmd: command;
}

(* [main ()] is the main function that starts the REPL by reading user input,
 * sending data to the server, and handling the server's response. *)
val main : unit -> unit

(* [parse s] is the command that gets parsed from a user's input string
 * [s]. Raises "Invalid command" if [s] is not a command. *)
val parse : string -> command

(* [create_user s] is the client output that will get sent to the server
 * when the user initiates his or her username as [s].
*)
val create_user : string -> client_output

(* [send_msg st s] returns the client output that will get sent to the server
 * when the user sends a message [s] based on the current state [st]. *)
val send_msg : state -> string -> client_output

(* [get_pulic_chats] returns the client output that will get sent to the
 * server when the user requests the list of public chats available to join in
 * based on the current state.
*)
val get_public_chats : state -> unit -> client_output

(* [get_online_users] returns the client output that will get sent to the server
 * when the user requests the list of current online users based on the current
 * state. *)
val get_online_users : state -> unit -> client_output

(* [get_curr_chats] is the client output that will get sent to the server
 * when the user requests the list of current chats, both private AND
 * public based on the current state. *)
val get_curr_chats : state -> unit -> client_output

(* [join_chat st s] returns the client output that will get sent to the server
 * when the user requests to join the chat by name [s] based on the current
 * state [st]. *)
val join_chat :  state -> string -> client_output

(* [change_chat st s] returns the client output that will get sent to the server
 * when the user requests to switch to a different chat by the name [s] based
 * on the current state [st]. *)
val change_chat : state -> string -> client_output

(* [help] is the string will get printed when the user requests for help using
 * the chat server *)
val help : unit -> string

(* [get_history] is the client output that will be sent to the server when
 * the user requests for past chatting messages, up to a limit of ten, based
 * on the current state. *)
val get_history : state -> unit -> client_output

(* [create_private_chat st s] is the client output that will be sent to the server
 * when the user requests to initiate a private chat with another user of
 * username [s] based on the current state [st]. *)
val create_private_chat : state -> string -> client_output

(* [create_group_chat st s] is the client output that will be sent to the server
 * when the user requests to initiate a group chat of chat name [s] based on
 * the currrent state [st]. *)
val create_group_chat : state -> string -> client_output

(* [leave_chat s] is the client output that will be sent to the server when
 * the user requests to leave a chat named [s]. *)
val leave_chat : state -> string -> client_output

(* [receive] takes the string received from server and returns the
 * [server_response] containing that data. *)
val receive_parse : string -> server_response

(* [send c] sends [c] of type [client_output] to the server. *)
val send : client_output -> unit

(* [receive] receives data from the server and returns it as a string *)
val receive : unit -> string
