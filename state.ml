open Data

type conn = Async.Reader.t * Async.Writer.t
type cid = int
type uid = int
type uname = string
type cname = string

exception UpdateError of string

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
module LD = ListDict

type state = {
  curr_conns: (uid, conn) LD.t;
  user_list: (uid, uname) LD.t;
  priv_chat_list: (cid, uid list) LD.t;
  pub_chat_list:(cid, uid list) LD.t;
  pub_chat_names: (cname, cid) LD.t;
  chat_msg: (cid, (uid*string) list) LD.t;
}

let init_state () = {
  curr_conns = LD.empty;
  user_list = LD.empty;
  priv_chat_list = LD.empty;
  pub_chat_list = LD.empty |> LD.insert 0 [];
  pub_chat_names = LD.empty |> LD.insert "Lobby" 0;
  chat_msg = LD.empty
}

let get_chats_of_uid st uid =
  let privs = st.priv_chat_list in
  let pubs = st.pub_chat_list in
  let rec get_cids id lst acc =
    match lst with
    | [] -> acc
    | (cid, ids)::t -> if List.mem id ids then get_cids id t (cid::acc)
      else get_cids id t acc in
  let check = List.rev_append (get_cids uid privs []) (get_cids uid pubs [] |> List.rev) in
  if check = [] then raise (UpdateError "User not found") else check

let get_priv_chats st =
  if st.priv_chat_list = [] then raise (UpdateError "No private chats")
  else st.priv_chat_list

let get_online_users st =
  let x = List.rev_map (fun x -> snd x) st.user_list in
  if x = [] then raise (UpdateError "No online users") else x

let get_pub_chats st =
  let x = List.rev_map (fun x -> fst x) st.pub_chat_names in
  if x = [] then raise(UpdateError "No public chats") else x

let get_users_of_chat st cid =
  try
    (let x = (try List.assoc cid st.pub_chat_list with _ -> List.assoc cid st.priv_chat_list) in
     if x = [] then raise (UpdateError "No users in chat") else x)
  with _-> raise (UpdateError "Error")

let get_conns_of_chat st chatid =
  try (
  let uids = get_users_of_chat st chatid in
  List.filter
    (fun (conn_uid, (conn_r, conn_w)) -> List.mem conn_uid uids) st.curr_conns)
  with _ -> raise (UpdateError "Error")

let get_history st cid =
  try
    (let msgs = List.assoc cid st.chat_msg in
     let rec f lst acc count = if count = 0 then acc else match lst with
         | [] -> acc
         | h::t -> f t (h::acc) (count-1) in
     f msgs [] 10)
  with _ -> raise (UpdateError "Chat not found")

let add_msg st uid (cid, msg) =
  try
    (let msgs = List.assoc cid st.chat_msg in
     let msgs' = (uid, msg) :: msgs in
     let dict' = LD.insert cid msgs' st.chat_msg in
     {st with chat_msg = dict'})
  with _ -> raise (UpdateError "Chat not found")

let add_user st uid uname =
  let open List in
  let user_list' = (uid, uname) :: if st.user_list |> split |> snd |> mem uname
                   then raise (UpdateError "Username taken, please try again.")
                   else st.user_list in
  {st with user_list = user_list'}

let add_conn st uid (r,w) =
  try (let conns' = LD.insert uid (r,w) st.curr_conns in
       {st with curr_conns = conns'})
  with _ -> raise (UpdateError "Error")

let add_pub_chat st uid chatid chatname =
  let chat_lst' = LD.insert chatid [uid] st.pub_chat_list in
  let chat_names' =
    (chatname, chatid) ::
    if List.mem_assoc chatname st.pub_chat_names
    then raise (UpdateError "Chat name taken, please try again.")
    else st.pub_chat_names in
  {st with pub_chat_list = chat_lst'; pub_chat_names = chat_names'}

let add_priv_chat st uid1 uid2 chatid =
  let chat_lst' = LD.insert chatid [uid1; uid2] st.priv_chat_list in
  {st with priv_chat_list = chat_lst'}

let add_user_to_pub_chat st uid cid =
  try (
    let user_lst = LD.get cid st.pub_chat_list in
    let user_lst' = uid :: user_lst in
    let chat_lst' = LD.insert cid user_lst' st.pub_chat_list in
    {st with pub_chat_list = chat_lst'})
  with _ -> raise (UpdateError "Chat not found")

let get_username st uid =
  try LD.get uid st.user_list with _ -> raise (UpdateError "User not found")

let get_uid st uname =
  try (
    let inv = Core.List.Assoc.inverse st.user_list in List.assoc uname inv)
  with _ -> raise (UpdateError ("User not found"))

let get_chatid st chatname =
  try LD.get chatname st.pub_chat_names with _ -> raise (UpdateError ("Chat not found"))

let remove_user st uid =
  let conns' = LD.remove uid st.curr_conns in
  let user_lst' = LD.remove uid st.user_list in
  let chat_rm lst = List.map (fun (cid, ulist) ->
      (cid, List.filter (fun x -> x <> uid) ulist)) lst in
  let priv_chat_lst' = chat_rm st.priv_chat_list in
  let pub_chat_lst' = chat_rm st.pub_chat_list in
  {st with curr_conns = conns'; user_list = user_lst';
           priv_chat_list = priv_chat_lst'; pub_chat_list = pub_chat_lst'}

let remove_from_chat st uid chatid =
  try (
  let users' = List.filter (fun x -> x <> uid) (get_users_of_chat st chatid) in
  if List.mem_assoc chatid st.pub_chat_list
  then {st with pub_chat_list = LD.insert chatid users' st.pub_chat_list}
  else {st with priv_chat_list = LD.insert chatid users' st.priv_chat_list})
  with _ -> raise (UpdateError "Chat not found")
