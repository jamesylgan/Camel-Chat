(* A type representing the information regarding the current "state"
 * on the client's repl:
 * - [userid] contains the id of the sending user.
 * - [curr_chatid] contains the user's current chat.
 * - [chats] contains all the chats the user is currently involved.
 * - [print] contians the stirng that should be printed out based on the user's
 *   most recent command.
 *)
type state = {
  userid : int;
  curr_chat : string * int;
  chats : (string * int) list;
  print: string list;
}

(* [init_state ()] returns the initial [state] for a client before initializing
 * a username. *)
val init_state : unit -> state

(* [get_userid st] returns the userid of a user in state [st]. *)
val get_userid : state -> int

(* [get_curr_chat st] returns the current chat name of a user in state [st]. *)
val get_curr_chat : state -> string

(* [get_print st] returns the string list that should be printed for a user
 * in state [st]. *)
val get_print : state -> string list

(* [get_chats st] returns a list of the chat names which a user in state
 * [st] has available. *)
val get_chats : state -> string list

(* [change_chat s st] returns the updated state when a user a state [st]
 * requests to switch to a chat by the name [s]. *)
val change_chat : string -> state -> state

(* [check_chat c st] checks if an user input [c] requests a legitimate name
 * for chat-related commands. If the chat name is illegit then a new state
 * with error message is returned, otherwise the original [st] is returned.
 *)
val check_chat : string -> state -> state

(* [parse_create_user s] returns the "client output" string for creating
 * the username [s]. *)
val parse_create_user : string -> string

(* [parse_send c st] returns the "output string" that would be sent to the
 * server based on the user's typed input [c] and current state [st].
 *)
val parse_send : string -> state -> string

(* [parse_receive r st] parses the "response string" [r] sent back by the
 * and returns an updated state based on the previous state [st].
 *)
val parse_receive : string -> state -> state

(*val extract_his : string -> string list -> string list*)
