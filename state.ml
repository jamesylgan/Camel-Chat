exception UpdateError of string

(* [lc] is the same as String.lowercase_ascii. It is an all-lowercase
 * representation of the argument.*)
let lc =  String.lowercase_ascii

(* [fst_lc lst] is the association list that is the same as [lst], except that
 * the first value in each pair has been changed to lowercase. *)
let fst_lc = List.map (fun (x,y) -> (lc x, y))

(* [snd_lc lst] is the association list that is the same as [lst], except that
 * the second value in each pair has been changed to lowercase. *)
let snd_lc = List.map (fun (x,y) -> (x, lc y))

type state = {
  curr_conns: (int * (Async.Reader.t * Async.Writer.t)) list;
  user_list: (int * string) list;
  priv_chat_list: (int * int list) list;
  pub_chat_list:(int * int list) list;
  pub_chat_names: (string * int) list;
  chat_msg: (int * (int*string) list) list;
}

(* [insert k v lst] produces a new association list [lst'] with the same
 * mappings as [lst] and also a mapping from [k] to [v].  If [k] was already
 * mapped in [lst], that mapping is replaced in [lst'] with the new mpaping. *)
let insert k v lst =
  (k, v) :: (if List.mem_assoc k lst then List.remove_assoc k lst else lst)

let init_state () = {
  curr_conns = [];
  user_list = [];
  priv_chat_list = [];
  pub_chat_list = [] |> insert 0 [];
  pub_chat_names = [] |> insert "Lobby" 0;
  chat_msg = [] |> insert 0 []
}

let get_chats_of_uid st uid =
  let privs = st.priv_chat_list in
  let pubs = st.pub_chat_list in
  let rec get_cids id lst acc =
    match lst with
    | [] -> acc
    | (cid, ids)::t -> if List.mem id ids then get_cids id t (cid::acc)
      else get_cids id t acc in
  let check = List.rev_append (get_cids uid privs [])
      (get_cids uid pubs [] |> List.rev) in
  if check = [] then raise (UpdateError "Error: User not found") else check

let get_priv_chats st =
  if st.priv_chat_list = [] then raise (UpdateError "Error: No private chats")
  else st.priv_chat_list

let get_online_users st =
  let x = List.rev_map (fun x -> snd x) st.user_list in
  if x = [] then raise (UpdateError "Error: No online users") else x

let get_pub_chats st =
  let x = List.rev_map (fun x -> fst x) st.pub_chat_names in
  if x = [] then raise(UpdateError "Error: No public chats") else x

let get_users_of_chat st cid =
  try (try List.assoc cid st.pub_chat_list with _ -> List.assoc cid st.priv_chat_list)
  with _-> raise (UpdateError "Error")

let get_conns_of_chat st chatid uid =
  try (
    let uids = get_users_of_chat st chatid in
    List.filter
    (fun (conn_uid, (conn_r, conn_w)) -> List.mem conn_uid uids && conn_uid <> uid) st.curr_conns)
  with _ -> raise (UpdateError "Error")

let get_history st cid =
  try
    (let msgs = List.assoc cid st.chat_msg in
     let rec f lst acc count = if count = 0 then acc else match lst with
         | [] -> acc
         | h::t -> f t (h::acc) (count-1) in
     f msgs [] 10)
  with _ -> raise (UpdateError "Error: Chat not found")

let add_msg st uid (cid, msg) =
  try
    (let msgs = List.assoc cid st.chat_msg in
     let msgs' = (uid, msg) :: msgs in
     let dict' = insert cid msgs' st.chat_msg in
     {st with chat_msg = dict'})
  with _ -> raise (UpdateError "Error: Chat not found")

let add_user st uid uname =
  let open List in
  let user_list' = (uid, uname) ::
                   if (List.map (fun (_,n) -> lc n) st.user_list) |> mem (lc uname)
                      || (List.mem_assoc (lc uname) (fst_lc st.pub_chat_names))
                   then raise (UpdateError "Error: Username taken, please try again.")
                   else st.user_list in
  {st with user_list = user_list'}

let add_conn st uid (r,w) =
  try (let conns' = insert uid (r,w) st.curr_conns in
       {st with curr_conns = conns'})
  with _ -> raise (UpdateError "Error")

let add_pub_chat st uid chatid chatname =
  let chat_lst' = insert chatid [uid] st.pub_chat_list in
  let chat_msg' = insert chatid [] st.chat_msg in
  let inv = Core.List.Assoc.inverse st.user_list in
  let chat_names' =
    (chatname, chatid) ::
    if List.mem_assoc (lc chatname) (fst_lc st.pub_chat_names)
       || List.mem_assoc (lc chatname) (fst_lc inv)
    then raise (UpdateError "Error: Chat name taken, please try again.")
    else st.pub_chat_names in
  {st with pub_chat_list = chat_lst'; pub_chat_names = chat_names';
           chat_msg = chat_msg'}

let add_priv_chat st uid1 uid2 chatid =
  let chat_lst' = insert chatid [uid1; uid2] st.priv_chat_list in
  let chat_msg' = insert chatid [] st.chat_msg in
  {st with priv_chat_list = chat_lst'; chat_msg = chat_msg'}

let add_user_to_pub_chat st uid cid =
  try (
  let user_lst = List.assoc cid st.pub_chat_list in
  let user_lst' = uid :: user_lst in
  let chat_lst' = insert cid user_lst' st.pub_chat_list in
  {st with pub_chat_list = chat_lst'})
  with _ -> raise (UpdateError "Error: Chat not found")

let is_username st chatname =
  List.fold_left (fun acc (uid, uname) ->
      if uname = chatname then acc else true || acc) false st.user_list

let get_username st uid =
  try List.assoc uid st.user_list with _ -> raise (UpdateError "Error: User not found")

let get_uid st uname =
  try (
    let inv = Core.List.Assoc.inverse st.user_list in List.assoc (lc uname) (fst_lc inv))
  with _ -> raise (UpdateError ("Error: User not found"))

let get_chatid st chatname =
  try List.assoc (lc chatname) (fst_lc st.pub_chat_names)
  with _ -> raise (UpdateError ("Error: Chat not found"))

let get_chat_info st chatname =
  try
    (let lst = List.filter (fun (cname, cid) -> lc cname = lc chatname)
         st.pub_chat_names @
               List.filter (fun (cname, cid) -> lc cname = lc chatname)
              st.pub_chat_names in
     match lst with
     | [] -> raise (UpdateError ("Error: Chat not found"))
     | h::t -> h )
  with _ -> raise (UpdateError ("Error: Chat not found"))

let remove_user st uid =
  let conns' = List.remove_assoc uid st.curr_conns in
  let user_lst' = List.remove_assoc uid st.user_list in
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
  then {st with pub_chat_list = insert chatid users' st.pub_chat_list}
  else {st with priv_chat_list = insert chatid users' st.priv_chat_list})
  with _ -> raise (UpdateError "Error: Chat not found")
