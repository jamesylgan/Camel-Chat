open State
open Async

type info = Nil | String of string | ISList of (int * string) list |
            Int of int | SList of string list | SSTuple of (string * string)

type command =
  | Create_user of string (* username *)
  | Send_msg of (int*string) (* chatid, msg*)
  | Get_public_chats
  | Get_online_users
  | Join_chat of string (* chatname*)
  | Get_history of int (* chatid *)
  | Create_priv_chat of string (* username of other user *)
  | Create_pub_chat of string (* chatname *)
  | Leave_chat of string (* chatname *)

type client_input = {
  userid: int;
  cmd: command;
}

type response = {
  userid: int;
  cmd: string;
  success: bool;
  info: info;
  chatid: int
}

type view_state = {
  state : State.state;
  uid : int;
  chatid : int;
  response: response option;
  res_string : string
}

(* [make_cmd cmd] is the alphabet associated with the command *)
let make_cmd = function
  | `SEND_MSG -> "a"
  | `GET_HISTORY -> "b"
  | `GET_USERS -> "c"
  | `CREATE_PRIV_CHAT -> "d"
  | `CREATE_PUB_CHAT -> "e"
  | `CREATE_USER -> "f"
  | `JOIN_CHAT -> "g"
  | `LEAVE_CHAT -> "h"
  | `GET_PUB_CHAT -> "i"
  | `RECEIVE_MSG -> "j"
  | `CHAT_NOTIF -> "k"

let b = "\027[0m"
let red = "\027[31m"
let green = "\027[32m"
let blue = "\027[34m"

let init_state () = {
  state = init_state ();
  uid = 0;
  chatid = 0;
  response = None;
  res_string = ""
}

(* A helper function that returns the index of last colon in a "client
 * output" string. *)
let last_c s =
  let open String in
  let n_of_chunk = function
    | 'a' -> 3
    | 'c' -> 1
    | 'f' -> 1
    | 'i' -> 1
    | o -> 2 in
  match n_of_chunk (get s 0) with
  | 1 -> index_from s 0 ':'
  | 2 -> begin
    let fst_c = index_from s 0 ':' in
    index_from s (fst_c + 1) ':'
  end
  | 3 -> begin
      let fst_c = index_from s 0 ':' in
      let snd_c = index_from s (fst_c + 1) ':' in
      let snd_comma = index_from s 2 ',' in
      let mes_len = (sub s (snd_comma + 2) (snd_c - snd_comma -2))
                    |> int_of_string in
      index_from s (snd_c + mes_len + 1) ':'
    end
  | _ -> failwith "Impossible n for n_of_chunk in server.ml"

let input_of_string s =
  let open String in
  let find_chat_name str =
    let identifier_index = (last_c s) + 1 in
    sub s (identifier_index) ((length s) - identifier_index) in
  let len_of_uid str =
    sub str 3 ((index_from str 3 ':')-3)
    |> int_of_string in
  let first_data str =
    sub str ((index_from str 3 ':') + 1) (len_of_uid str) in
  let response_uid str =
    first_data str
    |> int_of_string in
  let find_chat_id str =
    find_chat_name str |> int_of_string in
  match get s 0 with
  | 'a' -> begin
    let fst_c = index_from s 0 ':' in
    let snd_c = index_from s (fst_c + 1) ':' in
    let snd_comma = index_from s 2 ',' in
    let mes_len = (sub s (snd_comma + 2) (snd_c - snd_comma -2))
                  |> int_of_string in
    let last_comma = snd_c + mes_len + 1 in
      {userid = (response_uid s);
       cmd = Send_msg (find_chat_id s,
                       (sub s (snd_c+1) (last_comma-snd_c -1)))
                            }
  end
  | 'b' -> {userid = (response_uid s); cmd = Get_history (find_chat_id s)}
  | 'c' -> {userid = (response_uid s); cmd = Get_online_users}
  | 'd' -> {userid = (response_uid s); cmd = Create_priv_chat (find_chat_name s)}
  | 'e' -> {userid = (response_uid s); cmd = Create_pub_chat (find_chat_name s)}
  | 'f' -> {userid = -1; cmd = Create_user (find_chat_name s)}
  | 'g' -> {userid = (response_uid s); cmd = Join_chat (find_chat_name s)}
  | 'h' -> {userid = (response_uid s); cmd = Leave_chat (find_chat_name s)}
  | 'i' -> {userid = (response_uid s); cmd = Get_public_chats}
  | _ -> failwith "Invalid command ID"

(*let parse str =
 let input = input_of_string str in
 let res = match input.cmd with
   | Create_user s -> create_user s
   | Send_msg tup -> send_msg input.userid tup
   | Get_public_chats -> get_public_chat input.userid
   | Get_online_users -> get_users input.userid
   | Join_chat s -> join_chat input.userid s
   | Get_history chatid -> get_history input.userid chatid
   | Create_priv_chat username -> create_private_chat input.userid username
   | Create_pub_chat chatname -> create_pub_chat input.userid chatname
   | Leave_chat chatname -> leave_chat input.userid chatname in
 string_of_response res*)

let rec string_of_response res =
  let open String in
  let uid = ", " ^ (res.userid |> string_of_int |> length |> string_of_int)
            ^ ":" ^ (res.userid |> string_of_int) in
  let cid = ", " ^ (res.chatid |> string_of_int |> length |> string_of_int)
            ^ ":" ^ (res.chatid |> string_of_int) in
  let sl_len i =
    begin match i with
      | SList x -> List.length x
      | ISList x -> List.length x
      | _ -> failwith "Don't use this not on SList" end in
  let extract_tuple i =
    begin match i with
      | SSTuple i -> i
      | _ -> failwith "Don't use this not on tuple" end in
  let extract_info i =
    begin match i with
    | String i -> i
    | SList x ->
      let rec formlist lst str = begin match lst with
        | [] -> str
        | h::t -> formlist t (str ^ (length h |> string_of_int) ^ ":" ^ h)
      end in formlist x ""
    | ISList x ->
      let rec formlist lst str = begin match lst with
        | [] -> str
        | (junk_uid, h)::t -> formlist t (str ^ (length h |> string_of_int) ^ ":" ^ h)
      end in formlist x ""
    | _ -> failwith "Don't use this not on info" end in
  begin match res.success with
  | true -> create_success res uid cid sl_len extract_tuple extract_info
  | false -> "f: " ^ res.cmd ^ ", "
             ^ (length (extract_info res.info) |> string_of_int) ^ ":"
             ^ (extract_info res.info) end
and create_success res uid cid sl_len extract_tuple extract_info =
  let open String in
  begin match res.cmd with
  | "a" -> "s: a" ^ uid
  | "b" -> "s: b" ^ uid ^ ", " ^ (sl_len res.info |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "c" -> "s: c, " ^ (sl_len res.info |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "d" -> "s: d" ^ uid ^ cid ^ ", "
           ^ ((extract_info res.info) |> length |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "e" -> "s: e" ^ uid ^ cid ^ ", "
           ^ ((extract_info res.info) |> length |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "f" -> "s: f" ^ uid
  | "g" -> "s: g" ^ uid ^ cid ^ ", "
           ^ ((String.length (extract_info res.info) |> string_of_int)
              ^ ":" ^ (extract_info res.info))
  | "h" -> "s: h" ^ uid ^ cid ^ ", "
           ^ ((String.length (extract_info res.info) |> string_of_int)
              ^ ":" ^ (extract_info res.info))
  | "i" -> "s: i" ^ uid ^ ", " ^ (sl_len res.info |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "j" -> "s: j" ^ uid ^ cid ^ ", "
           ^ ((String.length (extract_info res.info) |> string_of_int)
              ^ ":" ^ (extract_info res.info))
  | "k" -> begin
    let chat_n = res.info |> extract_tuple |> fst in
    let msg = res.info |> extract_tuple |> snd in
    "s: k" ^ uid ^ cid ^ ", "
    ^ (chat_n |> String.length |> string_of_int) ^ ":"
    ^ chat_n ^ ", " ^ (msg |> String.length |> string_of_int) ^ ":" ^ msg
  end
  | _ -> failwith "Invalid input command"
end

let get_users st uid =
  try let users = get_online_users st.state in
    let res' = Some
        {userid = uid; cmd = make_cmd `GET_USERS; success = true;
         info = SList users; chatid = -1}
    in {st with response = res'}
  with UpdateError err ->
    let res' = Some
        {userid = uid; cmd = make_cmd `GET_USERS; success = false;
         info = String err; chatid = -1}
    in {st with response = res'}

(* does nothing if server broadcasting to disconnected client *)
let rec broadcast_to_chat st uid (chatid, msg) msg_or_notif =
  print_string ("broadcasting to chat " ^ string_of_int chatid ^ "\n");
  let conn_lst = get_conns_of_chat st.state chatid uid in
  let iter_helper chatid msg uid st (conn_uid,(_,w)) =
    print_string (string_of_int conn_uid ^ ": ");
    let res_msg =
      match msg_or_notif with
      | `MSG ->
        string_of_response {userid = conn_uid; cmd = make_cmd `RECEIVE_MSG;
                            success = true; info = String msg; chatid = chatid}
      | `NOTIF s ->
        string_of_response {userid = conn_uid; cmd = make_cmd `CHAT_NOTIF;
                            success = true; info = SSTuple (s, msg);
                            chatid = chatid} in
    print_string (res_msg ^ "\n");
    if Writer.is_open w then (Writer.write w (res_msg ^"\n"); st)
    else disconnected_client st uid conn_uid msg_or_notif in
  List.fold_left (iter_helper chatid msg uid) st conn_lst

and disconnected_client st uid conn_uid msg_or_notif =
  if uid = 0 then st (* server is broadcasting *)
  else
    (print_string ("client " ^ string_of_int conn_uid ^ " has disconnected\n");
     handle_disconnect st conn_uid)

and handle_disconnect st uid =
  try let user_chats = get_chats_of_uid st.state uid in
    let username = get_username st.state uid in
    let msg = " has left the chat." in
    let view_state' = List.fold_left
        (fun state cid ->
           (*st := add_msg !st 0 (cid, msg);*)
           broadcast_to_chat st 0 (cid,msg) (`NOTIF username)
        ) st user_chats in
    let state' = remove_user view_state'.state uid in
    {view_state' with state = state'}
  with UpdateError err -> print_string err; st

let join_chat st uid chatname =
  try let (cname, chatid) = get_chat_info st.state chatname in
    print_endline (string_of_int chatid);
    let username = get_username st.state uid in
    print_endline (username ^ " is joining chat " ^ cname);
    let state' = add_user_to_pub_chat st.state uid chatid in
    let res' = Some {userid = uid; cmd = make_cmd `JOIN_CHAT; success = true;
                info = String (cname); chatid = chatid} in
    let view_state' = {st with state = state'; response = res'} in
    broadcast_to_chat view_state' 0
      (chatid, (" has joined the chat")) (`NOTIF username)
with UpdateError err ->
    let res' = Some {userid = uid; cmd = make_cmd `JOIN_CHAT; success = false;
                     info = String err; chatid = -1} in
    {st with response = res'}

let leave_chat st uid chatname =
  try let chatid = get_chatid st.state chatname in
    let username = get_username st.state uid in
    print_endline (username ^ " is leaving chat " ^ chatname);
    let state' = remove_from_chat st.state uid chatid in
    let res' = Some {userid = uid; cmd = make_cmd `LEAVE_CHAT; success = true;
                     info = String (chatname); chatid = chatid} in
    let view_state' = {st with state = state'; response = res'} in
    broadcast_to_chat view_state' 0
             (chatid, (" has left the chat")) (`NOTIF username)
  with UpdateError err ->
    let res' = if is_username st.state chatname then
    Some {userid = uid; cmd = make_cmd `LEAVE_CHAT; success = false;
          info = String "You can't leave a private chat!"; chatid = -1} 
      else
    Some {userid = uid; cmd = make_cmd `LEAVE_CHAT; success = false;
                     info = String err; chatid = -1} in
    {st with response = res'}

let create_user st username r w =
  print_string "Creating new user\n";
  let new_uid = st.uid + 1 in
  try let state1 = add_user st.state new_uid username in
    let state2 = add_user_to_pub_chat state1 new_uid 0 in
    let state3 = add_conn state2 new_uid (r,w) in
    let res' = Some {userid = new_uid; cmd = make_cmd `CREATE_USER;
                     success = true; info = Nil; chatid = 0} in
    {st with state = state3; response = res'; uid = new_uid}
  with UpdateError err ->
    let res' = Some {userid = -1; cmd = make_cmd `CREATE_USER;
                     success = false; info = String err; chatid = -1} in
    {st with response = res'}

(* msg stored includes username *)
let send_msg st uid (chatid, msg) =
  print_endline ("msg received: " ^ msg ^ " for chat " ^ string_of_int chatid);
  try let username = get_username st.state uid in
    let new_msg = green ^ username ^ blue ^ ": " ^ msg ^ b in
    let state' = add_msg st.state uid (chatid, new_msg) in
    print_string ("added msg " ^ new_msg ^ "\n");
    let view_state = {st with state = state'} in
    let view_state' = broadcast_to_chat view_state uid (chatid, new_msg) `MSG in
    let res' = Some {userid = uid; cmd = make_cmd `SEND_MSG; success = true;
                     info = Nil; chatid = chatid} in
    {view_state' with response = res'}
  with UpdateError err ->
    print_endline ("send msg error: " ^ err);
    let res' = Some {userid = uid; cmd = make_cmd `SEND_MSG; success = false;
                     info = String err; chatid = -1} in
    {st with response = res'}

let get_history st uid chatid =
  try let history = get_history st.state chatid in
    let res' = Some {userid = uid; cmd = make_cmd `GET_HISTORY; success = true;
                     info = ISList history; chatid = chatid} in
    {st with response = res'}
  with UpdateError err ->
let res' = Some {userid = uid; cmd = make_cmd `GET_HISTORY; success = false;
                 info = String err; chatid = -1} in
    {st with response = res'}

let rec create_private_chat st uid username =
  try let accepting_uid = get_uid st.state username in
    let sender_username = get_username st.state uid in
    if username = sender_username
    then raise (UpdateError "You can't start a chat with yourself!")
    else
      begin
        let new_chatid = st.chatid + 1 in
        print_endline ("Creating priv chat with " ^ username ^ " of chatid " ^
                       string_of_int new_chatid);
        let state1 = add_priv_chat st.state uid accepting_uid new_chatid in
        let view_state1 = {st with state = state1} in
        let view_state2 =
          broadcast_to_chat view_state1 uid
          (new_chatid, (" has started a chat with you."))
          (`NOTIF sender_username) in
        let res' = Some {userid = uid; cmd = make_cmd `CREATE_PRIV_CHAT;
                         success = true; info = String (username);
                         chatid = new_chatid}
        in {view_state2 with response = res'; chatid = new_chatid}
      end
  with UpdateError err ->
    let res' = Some {userid = uid; cmd = make_cmd `CREATE_PRIV_CHAT;
                     success = false; info = String err; chatid = -1} in
    {st with response = res'}

let create_pub_chat st uid chatname =
  print_endline ("Creating pub chat " ^ chatname);
  let new_chatid = st.chatid + 1 in
  try let state' = add_pub_chat st.state uid new_chatid chatname in
    let res' = Some {userid = uid; cmd = make_cmd `CREATE_PUB_CHAT;
                     success = true; info = String (chatname);
                     chatid = new_chatid}
    in {st with response = res'; state = state'; chatid = new_chatid}
  with UpdateError err ->
    let res' = Some {userid = uid; cmd = make_cmd `CREATE_PUB_CHAT;
                     success = false; info = String err; chatid = -1} in
    {st with response = res'}

let get_public_chat st uid =
  try let pub_chats = get_pub_chats st.state in
    let res' = Some {userid = uid; cmd = make_cmd `GET_PUB_CHAT;
                     success = true; info = SList pub_chats; chatid = -1} in
    {st with response = res'}
  with UpdateError err ->
    let res' = Some {userid = uid; cmd = make_cmd `GET_PUB_CHAT;
                     success = false; info = String err; chatid = -1} in
    {st with response = res'}

let parse st str r w =
  let input = input_of_string str in
  let res = match input.cmd with
    | Create_user s -> create_user st s r w
    | Send_msg tup -> send_msg st input.userid tup
    | Get_public_chats -> get_public_chat st input.userid
    | Get_online_users -> get_users st input.userid
    | Join_chat s -> join_chat st input.userid s
    | Get_history chatid -> get_history st input.userid chatid
    | Create_priv_chat username ->
      create_private_chat st input.userid username
    | Create_pub_chat chatname -> create_pub_chat st input.userid chatname
    | Leave_chat chatname -> leave_chat st input.userid chatname in
  match res.response with
  | None -> failwith "impossible"
  | Some s -> {res with res_string = string_of_response s}
