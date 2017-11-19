open data
(* [state] is the current state of the server. The server maintains a 4
 * dictionaries:
   - curr_conns maps uid to the (Reader, Writer) object of the connection
   - user_list maps userid to usernames
   - priv_chat_list maps each private chatid to a list of (userid, userid) tuple
   - pub_chat_list maps each public chatid to a list of userids
   - chat_msg maps each chatid to a (int * string) list (ie userid, messages of
     the chat)
   - pub_chat_names maps the name of a public chat to its chatid
   - chat_msg maps the chatid to a list of messages that was sent in the chat
*)
(* NOTE: dictionaries should be somehow changed to hash maps? *)
type state = {
  curr_conns: Dictionary;
  user_list: Dictionary;
  priv_chat_list: Dictionary;
  pub_chat_list: Dictionary;
  pub_chat_names: Dictionary;
  chat_msg: Dictionary;
}

(* [init_state] initializes a blank state with empty dictionaries *)
val init_state: unit -> state

(* [get_chats_of_uid uid] is the list of chatids that the uid is in (both)
 * public and private chats *)
val get_chats_of_uid: state -> int -> int list

(* [get_conns_of_chat st uid chatid] gets all uids (that are not equal to [uid])
 * and are in st.pub_chat_list  or st.priv_chat_list and then gets the (r,w)
 * tuple associated with the uids as listed in st.curr_conns *)
val get_conns_of_chat: state -> int -> (Async.Reader * Async.Writer) list

(* [get_priv_chats st] is st.priv_chat_lists *)
val get_priv_chats: state -> Dictionary

(* [get_online_users st] is the string list of all online users *)
val get_online_users: state -> string list

(* [get_pub_chat_name st] is the list of all public chat names *)
val get_pub_chats: state -> string list

(* [get_users_of_chat] gets the list of users of [chatid] in st.pub_chat_list
 * or st.priv_chat_list *)
val get_users_of_chat: state -> int -> int list

(* get the last 10 chat messages in the chat *)
val get_history: state -> int -> (int * string) list

(* [add_msg st chatid (uid, msg)] adds the chat message to st.chat_msg  *)
val add_msg: state -> int -> (int * string) -> state

(* [add_user st uid username] adds [uid] to st.user_list with [username] *)
val add_user: state -> int -> string -> state

(* [add_conn st uid (r,w)] adds (r,w) to st.curr_conns *)
val add_conn: state -> int -> (Async.Reader * Async.Writer) -> state

(* [add_pub_chat uid chatid chatname] updates st.pub_chat_list with [uid],
 * and updates st.chat_names with [chatname]. [chatname] is a tuple  *)
val add_pub_chat: state -> int -> int -> string -> state

(* [add_priv_chat uid1 uid2 chatid] updates st.priv_chat_list with [uid1] and
 * [uid2] in [chatid] *)
val add_priv_chat: state -> int -> int -> int -> state

(* [add_user_to_pub_chat uid chatid] adds [uid] to the list of users of [chatid]
 * in st.pub_chat_list *)
val add_user_to_pub_chat: state -> int -> int -> state

(* [get_username st uid] is the username of [uid] as listed in st.user_list *)
val get_username: state -> int -> string

(* [get_chatid st chatname] is the chatid associated with [chatname] in
 * st.pub_chat_names *)
val get_chatid: state -> string -> int

(* [remove_user st uid] removes [uid] from st.curr_conns, st.user_list, and both
 * chat lists *)
val remove_user: state -> int -> state
