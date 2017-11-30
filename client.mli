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
  curr_chatid : int;
  chats : int list;
  print: string list;
}

(* [parse_send c st] returns the "output string" that would be sent to the
 * server based on the user's typed input [c] and current state [st].
 *)
val parse_send : string -> state -> string

(* [parse_receive r st] parses the "response string" [r] sent back by the
 * and returns an updated state based on the previous state [st].
 *)
val parse_receive : string -> state -> state

val extract_his : string -> string list -> string list
