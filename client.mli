type info

type command

type state = {
  userid : int;
  curr_chatid : int
}

type server_response = {
  success : bool;
  info : string
}

type client_output = {
  userid : int;
  cmd: command;
  info: info
}

val main : unit -> unit

(* [parse s] is the client command that gets parsed from a user's input string
 * [s]. Raises "Invalid command" if [s] is not a command. *)
val parse : string -> command

(* [create_user c] is the state that results from updating .   *)
val create_user : string -> client_output

val send_msg : string -> client_output

val get_public_chats : unit -> client_output

val get_online_users : unit -> client_output

val get_curr_chats : unit -> client_output

val join_chat : string -> client_output

val change_chat : string -> client_output

val help : unit -> client_output

val get_history : unit -> client_output

val create_private_chat : string -> client_output

val create_group_chat : string -> client_output

val leave_chat : string -> client_output

val send : client_output -> unit

val receive : unit -> server_response
