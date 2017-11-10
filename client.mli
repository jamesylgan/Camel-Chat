type info

type state = {
  userid : info;
  curr_chatid : int
}

type server_response = {
  success : bool;
  info : string
}

type client_command = {
  userid : int;
  cmd: string;
  info: string
}

val main : unit -> unit

val parse : string -> client_command

val create_user : client_command -> state

val send : client_command -> unit

val receive : unit -> server_response

val send_msg : client_command -> unit

val get_public_chats : client_command -> unit

val get_online_users : client_command -> unit

val get_curr_chats : client_command -> unit

val join_chat : client_command -> state

val help : client_command -> unit

val get_history : client_command -> unit

val create_private_chat : client_command -> state

val create_group_chat : client_command -> state

val leave_chat : client_command -> state
