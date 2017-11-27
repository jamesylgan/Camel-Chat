open Data

type state

exception UpdateError of string

(* [init_state] initializes a blank state with empty dictionaries *)
val init_state: unit -> state

(* [get_chats_of_uid st uid chatid] is the list of chatids that the uid is in
 * (both) public and private chats *)
val get_chats_of_uid: state -> int -> int list

(* [get_conns_of_chat st uid chatid] gets all uids (that are not equal to [uid])
 * and are in st.pub_chat_list or st.priv_chat_list and then gets the (r,w)
 * tuple associated with the uids as listed in st.curr_conns *)
val get_conns_of_chat: state -> int -> int -> (Async.Reader.t * Async.Writer.t) list

(* [get_priv_chats st] is st.priv_chat_lists *)
val get_priv_chats: state -> (int, int list) ListDict.t

(* [get_online_users st] is the string list of all online users *)
val get_online_users: state -> string list

(* [get_pub_chat_name st] is the list of all public chat names *)
val get_pub_chats: state -> string list

(* [get_users_of_chat st chatid] gets the list of users of [chatid] in
 * st.pub_chat_list or st.priv_chat_list. Raises Not_found if [chatid] is not
 * a current chat id. *)
val get_users_of_chat : state -> int -> int list

(* get the last 10 chat messages in the chat *)
val get_history: state -> int -> (int * string) list

(* [add_msg st uid (chatid, msg)] adds the chat message to st.chat_msg  *)
val add_msg: state -> int -> (int * string) -> state

(* [add_user st uid username] adds [uid] to st.user_list with [username]
 * -raises: UpdateError when username already exists. *)
val add_user: state -> int -> string -> state

(* [add_conn st uid (r,w)] adds (r,w) to st.curr_conns *)
val add_conn: state -> int -> (Async.Reader.t * Async.Writer.t) -> state

(* [add_pub_chat st uid chatid chatname] updates st.pub_chat_list with [uid],
 * and updates st.chat_names with [chatname]. [chatname] is a tuple  *)
val add_pub_chat: state -> int -> int -> string -> state

(* [add_priv_chat st uid1 uid2 chatid] updates st.priv_chat_list with [uid1] and
 * [uid2] in [chatid] *)
val add_priv_chat: state -> int -> int -> int -> state

(* [add_user_to_pub_chat st uid chatid] adds [uid] to the list of users of [chatid]
 * in st.pub_chat_list *)
val add_user_to_pub_chat: state -> int -> int -> state

(* [get_username st uid] is the username of [uid] as listed in st.user_list *)
val get_username: state -> int -> string

(* [get_uid st username] is the username of [uid] as listed in st.user_list *)
val get_uid: state -> string -> int

(* [get_chatid st chatname] is the chatid associated with [chatname] in
 * st.pub_chat_names *)
val get_chatid: state -> string -> int

(* [remove_user st uid] removes [uid] from st.curr_conns, st.user_list, and both
 * chat lists *)
val remove_user: state -> int -> state

(* [remove_from_chat st uid chatid] removes [uid] from [chat] *)
val remove_from_chat: state -> int -> int -> state
