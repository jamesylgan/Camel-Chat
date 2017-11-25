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
  chat_msg: (cid, (int*string) list) LD.t;
}

let init_state () = {curr_conns = LD.empty;
                     user_list = LD.empty;
                     priv_chat_list = LD.empty;
                     pub_chat_list = LD.empty;
                     pub_chat_names = LD.empty;
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
  List.rev_append (get_cids uid privs []) (get_cids uid pubs [] |> List.rev)

let get_conns_of_chat st uid chatid = failwith "Unimplemented"

let get_priv_chats st = st.priv_chat_list

let get_online_users st = List.rev_map (fun x -> snd x) st.user_list

let get_pub_chats st = List.rev_map (fun x -> fst x) st.pub_chat_names

let get_users_of_chat st cid =
  try List.assoc cid st.pub_chat_list with _ -> List.assoc cid st.priv_chat_list

let get_history st cid =
  let msgs = List.assoc cid st.chat_msg in
  let rec f lst acc count = if count = 0 then acc else match lst with
    | [] -> acc
    | h::t -> f t (h::acc) (count-1) in
  f msgs [] 10

let add_msg st uid (cid, msg) =
  let msgs = List.assoc cid st.chat_msg in
  print_string "removing \n";
  failwith "Unimplemented"

let add_user st uid username = failwith "Unimplemented"

let add_conn st uid = failwith "Unimplemented"

let add_pub_chat st uid chatid chatname = failwith "Unimplemented"

let add_priv_chat st uid1 uid2 chatid= failwith "Unimplemented"

let add_user_to_pub_chat st uid chatid = failwith "Unimplemented"

let get_username st uid = failwith "Unimplemented"

let get_chatid st chatname = failwith "Unimplemented"

let remove_user st uid = failwith "Unimplemented"

let remove_from_chat st uid chatid = failwith "Unimplemented"

let get_uid st username = failwith "Unimplemented"
