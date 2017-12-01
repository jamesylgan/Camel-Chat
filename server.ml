open State
open Async

type info = Nil | String of string | ISList of (int * string) list |
            Int of int | SList of string list | ISTuple of (int * string)

let st = ref (init_state ())
let uid = ref (0)
let next_uid = fun () -> uid := (!uid) + 1; !uid
let prev_uid = fun () -> uid := (!uid) - 1; !uid
let chatid = ref (0)
let next_chatid = fun () -> chatid := (!chatid) + 1; !chatid
let prev_chatid = fun () -> chatid := (!chatid) - 1; !chatid

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
                            (sub s 0 last_comma)
                            |> find_chat_name)}
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
    | ISTuple (c_id, c_name) -> ", " ^ (c_id |> string_of_int
                                      |> length |> string_of_int)
                              ^ ":" ^ (c_id |> string_of_int)
                              ^ ", " ^ (c_name |> length |> string_of_int)
                              ^ ":" ^ (c_name)
    | _ -> failwith "Don't use this not on info" end in
  begin match res.success with
  | true -> create_success res uid cid sl_len extract_info
  | false -> "f: " ^ res.cmd ^ ", "
             ^ (length (extract_info res.info) |> string_of_int) ^ ":"
             ^ (extract_info res.info) end
and create_success res uid cid sl_len extract_info = begin match res.cmd with
  | "a" -> "s: a" ^ uid
  | "b" -> "s: b" ^ uid ^ ", " ^ (sl_len res.info |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "c" -> "s: c, " ^ (sl_len res.info |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "d" -> "s: d" ^ uid ^ cid ^ (extract_info res.info)
  | "e" -> "s: e" ^ uid ^ cid ^ (extract_info res.info)
  | "f" -> "s: f" ^ uid
  | "g" -> "s: g" ^ uid ^ cid ^ ", "
           ^ ((String.length (extract_info res.info) |> string_of_int)
              ^ ":" ^ (extract_info res.info))
  | "h" -> "s: h" ^ uid ^ cid ^ ", "
           ^ ((String.length (extract_info res.info) |> string_of_int)
              ^ ":" ^ (extract_info res.info))
  | "i" -> "s: i" ^ uid ^ ", " ^ (sl_len res.info |> string_of_int)
           ^ ":" ^ (extract_info res.info)
  | "j" -> "s: j" ^ uid ^ cid ^ ", " ^ ", "
           ^ ((String.length (extract_info res.info) |> string_of_int)
              ^ ":" ^ (extract_info res.info))
  | _ -> failwith "Invalid input command"
end

let get_users uid =
  try let users = get_online_users !st in
    {userid = uid; cmd = "c"; success = true; info = SList users; chatid = -1}
  with UpdateError err ->
    {userid = uid; cmd = "c"; success = false; info = String err; chatid = -1}

let join_chat uid chatname =
  let chatid = get_chatid !st chatname in
  try st := add_user_to_pub_chat !st uid chatid;
        {userid = uid; cmd = "g"; success = true;
         info = String (chatname); chatid = chatid}
  with UpdateError err ->
    {userid = uid; cmd = "g"; success = false; info = String err; chatid = -1}

let leave_chat uid chatname =
  let chatid = get_chatid !st chatname in
  try st := remove_from_chat !st uid chatid;
    {userid = uid; cmd = "h"; success = true;
     info = String (chatname); chatid = chatid}
  with UpdateError err ->
    {userid = uid; cmd = "h"; success = false; info = String err; chatid = -1}

let create_user username =
  print_string "create uuser\n";
  let new_uid = next_uid () in
  try st := add_user !st new_uid username;
    st := add_user_to_pub_chat !st new_uid 0;
    print_string "success\n";
    {userid = new_uid; cmd = "f"; success = true; info = Nil; chatid = -1}
  with UpdateError err ->
    let _ = prev_uid () in
    {userid = -1; cmd = "f"; success = false; info = String err; chatid = -1}

(* does nothing if server broadcasting to disconnected client *)
let rec broadcast_to_chat uid (chatid, msg) =
  let rwLst = get_conns_of_chat !st chatid in
  List.iter
    (fun (conn_uid,(_,w)) ->
       let resp_msg = string_of_response
           {userid = conn_uid; cmd = "j"; success = true;
            info = String msg; chatid = chatid} in
       if Writer.is_open w then Writer.write w resp_msg
       else disconnected_client uid conn_uid) rwLst

and disconnected_client uid conn_uid =
  if uid = 0 then () (* server is broadcasting *)
  else
    (print_string ("client " ^ string_of_int conn_uid ^ " has disconnected");
     handle_disconnect conn_uid)

and handle_disconnect uid =
  try let user_chats = get_chats_of_uid !st uid in
    let username = get_username !st uid in
    let msg = username ^ " has left." in
    List.iter
      (fun cid ->
         st := add_msg !st 0 (cid, msg);
         broadcast_to_chat 0 (cid,msg)
      ) user_chats;
    st := remove_user !st uid; ()
  with UpdateError err -> print_string err; ()

(* msg stored includes username *)
let send_msg uid (chatid, msg) =
  try let username = get_username !st uid in
    let new_msg = username ^ ": " ^ msg in
    st := add_msg !st uid (chatid, new_msg);
    ignore (broadcast_to_chat uid (chatid, new_msg));
    {userid = uid; cmd = "a"; success = true; info = Nil; chatid = chatid}
  with UpdateError err ->
    {userid = uid; cmd = "a"; success = false; info = String err; chatid = -1}

let get_history uid chatid =
  try let history = get_history !st chatid in
    {userid = uid; cmd = "b"; success = true; info = ISList history;
     chatid = chatid}
  with UpdateError err ->
    {userid = uid; cmd = "b"; success = false; info = String err; chatid = -1}

let create_private_chat uid username =
  let uid2 = get_uid !st username in
  let new_chatid = next_chatid () in
  try st := add_priv_chat !st uid uid2 new_chatid;
    {userid = uid; cmd = "d"; success = true;
     info = ISTuple (new_chatid, username); chatid = new_chatid}
  with UpdateError err ->
    let _ = prev_chatid () in
    {userid = uid; cmd = "d"; success = false; info = String err; chatid = -1}

let create_pub_chat uid chatname =
  let new_chatid = next_chatid () in
  try st := add_pub_chat !st uid new_chatid chatname;
    {userid = uid; cmd = "e"; success = true;
     info = ISTuple (new_chatid, chatname); chatid = new_chatid}
  with UpdateError err ->
    let _ = prev_chatid () in
    {userid = uid; cmd = "e"; success = false; info = String err; chatid = -1}

let get_public_chat uid =
  try let pub_chats = get_pub_chats !st in
    {userid = uid; cmd = "i"; success = true; info = SList pub_chats;
     chatid = -1}
  with UpdateError err ->
  {userid = uid; cmd = "i"; success = false; info = String err; chatid = -1}

let parse str =
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
  string_of_response res

(*let parse x = print_string ("parsing " ^ x); (x ^ "vvv")*)
let handle_connection _addr r w =
  let () = print_string ("New client \n") in
  let rec loop r w =
    Reader.read_line r >>= function
    | `Eof -> (printf "Error reading server\n"; return ())
    | `Ok line -> (print_endline ("received: " ^ line);
                   Writer.write_line w (parse line);
                   loop r w)
in loop r w
  (*Pipe.transfer (Reader.pipe r) (Writer.pipe w)
    (fun x -> parse x)*)

let quit_regex = Str.regexp {|^#quit\(;;\)?$|}

let matches s r =
  Str.string_match r s 0

let handle_stdin input =
  if matches input quit_regex then let _ = exit 0 in ()
  else print_string "Invalid command\n"; ()

let rec read_cmdline () =
  let stdin : Reader.t = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= fun res -> ignore(
    begin
      match res with
      | `Ok str -> return (handle_stdin str)
      | `Eof ->  return ()
    end);
  ignore (read_cmdline());
  Deferred.never ()

let create_tcp ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w -> handle_connection _addr r w) in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let run ~port =
  ignore (read_cmdline ());
  ignore (create_tcp port);
  Deferred.never ()

let main () =
  print_string "Starting chat server... \n";
  print_string "Enter \"#quit\" to shutdown the server. \n";
  print_string  "> ";
  Command.async
    ~summary:"Start the chat server"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 9999 int)
        ~doc:" Port to listen on (default 9999)"
    )
    (fun port () -> run ~port)
  |> Command.run

let () = main ()
