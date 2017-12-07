
(* [state] is the current state of the server. The server maintains multiple
 * dictionaries as association lists:
   - curr_conns maps uid to the (Reader, Writer) object of the connection
   - user_list maps userid to usernames
   - priv_chat_list maps each private chatid to a list of (userid, userid) tuple
   - pub_chat_list maps each public chatid to a list of userids
   - chat_msg maps each chatid to a (int * string) list (ie userid, messages of
     the chat)
   - pub_chat_names maps the name of a public chat to its chatid
   - chat_msg maps the chatid to a list of messages that was sent in the chat
*)
type state = {
    curr_conns: (int * (Async.Reader.t * Async.Writer.t)) list;
    user_list: (int * string) list;
    priv_chat_list: (int * int list) list;
    pub_chat_list:(int * int list) list;
    pub_chat_names: (string * int) list;
    chat_msg: (int * (int*string) list) list;
  }

exception UpdateError of string

(* [init_state] initializes the beginning state with only a public chat named
 * lobby with id 0. *)
val init_state: unit -> state

(* [get_chats_of_uid st uid] is the list of chatids that the [uid] is in
 * (both) public and private chats.
 * -raises: UpdateError "User not found" if [uid] is not in any chats. *)
val get_chats_of_uid: state -> int -> int list

(* [get_conns_of_chat st chatid uid] gets all uids and conn_uid <> uid
 * and are in st.pub_chat_list or st.priv_chat_list and then gets the (r,w)
 * tuple associated with the uids as listed in st.curr_conns
 * -raises: UpdateError "Error" if [chatid] is not a current chat id or if there
 *          are no users in [chatid]. *)
val get_conns_of_chat: state -> int -> int ->
  (int * (Async.Reader.t * Async.Writer.t)) list

(* [get_priv_chats st] is st.priv_chat_lists.
 * -raises: UpdateError "No private chats" if st.priv_chat_lists is empty. *)
val get_priv_chats: state -> (int * int list) list

(* [get_online_users st] is the string list of all online users.
 * -raises: UpdateError "No online users" if the user_list is empty. *)
val get_online_users: state -> string list

(* [get_pub_chat_name st] is the list of all public chat names.
 * -raises: UpdateError "No public chats" if pub_chat_names is empty. *)
val get_pub_chats: state -> string list

(* [get_users_of_chat st chatid] gets the list of users of [chatid] in
 * st.pub_chat_list or st.priv_chat_list.
 * -raises: UpdateError "Error" if [chatid] is not a current chat id or if there
 *          are no users in [chatid]. *)
val get_users_of_chat : state -> int -> int list

(* [get_history st chatid] gets the last 10 chat messages in the chat, in order
 * of oldest to newest.
 * -raises: UpdateError "Chat not found" if [chatid] is not a current chat id.*)
val get_history: state -> int -> (int * string) list

(* [add_msg st uid (chatid, msg)] adds the chat message to st.chat_msg.
 * -raises: UpdateError "Chat not found" if [chatid] is not a current chat id.*)
val add_msg: state -> int -> (int * string) -> state

(* [add_user st uid username] adds [uid] to st.user_list with [username]
 * -raises: UpdateError "Username taken, please try again" when username already
 *          exists. *)
val add_user: state -> int -> string -> state

(* [add_conn st uid (r,w)] adds (r,w) to st.curr_conns. *)
val add_conn: state -> int -> (Async.Reader.t * Async.Writer.t) -> state

(* [add_pub_chat st uid chatid chatname] updates st.pub_chat_list with [uid],
 * and updates st.chat_names with [chatname]. Also updates st.chat_msg with
 * (chatid, []).
 * -raises: UpdateError "Chat name taken, please try again" when chat name or
 *          username already exists. *)
val add_pub_chat: state -> int -> int -> string -> state

(* [add_priv_chat st uid1 uid2 chatid] updates st.priv_chat_list with [uid1] and
 * [uid2] in [chatid] and initializes st.chat_msg with (cid, []). Also updates
 * st.chat_msg with (chatid, []). *)
val add_priv_chat: state -> int -> int -> int -> state

(* [add_user_to_pub_chat st uid chatid] adds [uid] to the list of users of
 * [chatid] in st.pub_chat_list and initializes st.chat_msg with (cid, []).
 * -raises: UpdateError "Chat not found" if [chatid] is not a current chat id.*)
val add_user_to_pub_chat: state -> int -> int -> state

(* [get_username st uid] is the username of [uid] as listed in st.user_list.
 * -raises: UpdateError "User not found" if [uid] is not in st.user_list. *)
val get_username: state -> int -> string

(* [get_uid st username] is the username of [uid] as listed in st.user_list.
* -raises: UpdateError "User not found" if [username] is not in st.user_list.*)
val get_uid: state -> string -> int

(* [get_chatid st chatname] is the chatid associated with [chatname] in
 * st.pub_chat_names st.priv_chat_list
 * -raises: UpdateError "Chat not found" if [chatid] is not a current chat id.*)
val get_chatid: state -> string -> int

(* [remove_user st uid] removes [uid] from st.curr_conns, st.user_list, and both
 * chat lists *)
val remove_user: state -> int -> state

(* [remove_from_chat st uid chatid] removes [uid] from [chat].
 * -raises: UpdateError "Chat not found" if [chatid] is not a current chat id.*)
val remove_from_chat: state -> int -> int -> state

(* [get_chat_info st chatname] is a tuple of the chatname and chatid *)
val get_chat_info: state -> string -> (string * int)

(* [is_username st uname] is true if [uname] is a username; false otherwise. *)
val is_username : state -> string -> bool
