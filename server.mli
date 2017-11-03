val main

val parse

    (* packet: cmd; userid; chatid; msg *)
val receive
cmds:
    - add_msg
    - get_history
    - get_users
    - create_private_chat
    - create_group_chat
    - get_public_groups
    - create_user
    - get_curr_chats
    - get_chat
    - leave_chat
    - join_chat

(* add userid to list of users in chat, send last 10 messages *)
val join_chat

(*if no one left in chat then then remove the entry *)
val leave_chat

val add_user

val remove_user

val send

(* remove disconnected user from chat lists
 * broadcast message to all chats that the user is in: "user _ has left" *)
val handle_disconnect

val broadcast_to_chat

val get_username (* broadcasting message based on username *)

val add_msg

val get_history

val create_private_chat (* send message other client *)

val create_group_chat

val get_public_groups

val get_curr_chats

val create_user

(* when users enter a chat, send the last 10 messages *)
val get_chat

val get_users
