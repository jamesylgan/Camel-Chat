type state = {
  userid : int;
  curr_chat : string * int;
  chats : (string * int) list;
  print: string list;
}

let init_state () =
  {
    userid = -1;
    curr_chat = ("not initialized", -1);
    chats = [];
    print = [];
  }

let red = "\027[31m"
let blue = "\027[34m"
let green = "\027[32m"
let purp = "\027[35m"
let cyan = "\027[36m"

let color c lst = List.map (fun x -> c ^ x) lst


let get_userid st = st.userid

let get_curr_chat st = st.curr_chat |> fst

let get_print st = st.print

let get_chats st = st.chats |> List.map fst

let change_chat s st =
  if not (List.mem_assoc s st.chats) then
    {st with print = ["#CHANGE_CHAT failed: You are not in chat " ^ purp ^ s ^ red ^ "."]}
  else if fst st.curr_chat = s then
    {st with print = ["#CHANGE_CHAT failed: You are already in the chat."]}
  else {
    userid = st.userid;
    curr_chat = (s, (List.assoc s st.chats));
    chats = st.chats;
    print = [red ^ "Entering chat " ^ purp ^ s ^ red ^ "..."];
  }

let check_chat s st =
  let open String in
  let join_chat = Str.regexp "#join \\(.+\\)" in
  let priv_chat = Str.regexp "#chatwith \\(.+\\)" in
  let pub_chat = Str.regexp "#makechat \\(.+\\)" in
  let leave_chat = Str.regexp "#leave \\(.+\\)" in
  if (Str.string_match priv_chat s 0) || (Str.string_match pub_chat s 0)
  then let name = sub s 10 ((length s) - 10) in
    if not (contains name ' ') then st
    else {st with print = [red ^ "Error: Please use a chat name without spaces!"]}
  else if (Str.string_match join_chat s 0)
  then let name = sub s 6 ((length s) - 6) |> String.lowercase_ascii in
    if not (List.mem_assoc name st.chats) then st
    else {st with print = [red ^ "Error: You are already in the chat!"]}
  else if (Str.string_match leave_chat s 0)
  then let name = sub s 7 ((length s) - 7) |> String.lowercase_ascii in
    if (List.mem_assoc name st.chats) then st
    else if name = "lobby" then {st with print = [red ^ "Error: You can't leave the lobby!"]}
    else {st with print = [red ^ "Error: You are not in chat " ^ purp ^ name]}
  else st

let parse_create_user s =
  "f, " ^ (String.length s |> string_of_int) ^ ":" ^ s

let parse_send s st =
  let open String in
  let uid = ", " ^ (st.userid |> string_of_int |> length |> string_of_int)
            ^ ":" ^ (st.userid |> string_of_int) in
  let chatid = ", " ^ (snd st.curr_chat |> string_of_int |> length |>
                       string_of_int)
               ^ ":" ^ (snd st.curr_chat |> string_of_int) in
  match s with
  (* Does not include [Help], [Create_user], and [Quit] which are
     managed in [View.ml]. *)
  | "#history" -> "b" ^ uid ^ chatid
  | "#users" -> "c" ^ uid
  | "#pubchats" -> "i" ^ uid
  | s -> begin
      let priv_chat = Str.regexp "#chatwith \\(.+\\)" in
      let pub_chat = Str.regexp "#makechat \\(.+\\)" in
      let join_chat = Str.regexp "#join \\(.+\\)" in
      let leave_chat = Str.regexp "#leave \\(.+\\)" in
      if Str.string_match priv_chat s 0
      then "d" ^ uid ^ ", " ^ ((length s)-10 |> string_of_int) ^
           ":" ^ ((length s)-10 |> sub s 10)
      else if Str.string_match pub_chat s 0
      then "e" ^ uid ^ ", " ^ ((length s)-10 |> string_of_int) ^
           ":" ^ ((length s)-10 |> sub s 10)
      else if Str.string_match join_chat s 0
      then "g" ^ uid ^ ", " ^ ((length s)-6 |> string_of_int) ^
           ":" ^ ((length s)-6 |> sub s 6)
      else if Str.string_match leave_chat s 0
      then "h" ^ uid ^ ", " ^ ((length s)-7 |> string_of_int) ^
           ":" ^ ((length s)-7 |> sub s 7)
      else "a" ^ uid ^ ", " ^ (s |> length |> string_of_int) ^
           ":" ^ s ^ chatid
    end

(* A helper function that returns the actual command corresponding to
 * a command_id. *)
let get_c = function
  | 'a' -> "#SEND_MSG"
  | 'b' -> "#GET_HISTORY"
  | 'c' -> "#GET_ONLINE_USERS"
  | 'd' -> "#CREATE_PRIV_CHAT"
  | 'e' -> "#CREATE_PUB_CHAT"
  | 'f' -> "#CREATE_USER"
  | 'g' -> "#JOIN_CHAT"
  | 'h' -> "#LEAVE_CHAT"
  | 'i'-> "#GET_PUB_CHAT"
  | others -> failwith "Invalid command id"

(* A helper function for [parse_receive] that extracts infromation from
 * string of the format "<len>:<name>". *)
let rec extract s acc =
  let open String in
  match s with
  | "" -> acc
  | s -> begin
    let c_loc = index_from s 0 ':' in
    let len = (sub s 0 c_loc) |> int_of_string in
    let m = (sub s (c_loc+1) len) in
    let new_s = sub s (c_loc + len + 1) ((length s) - c_loc - len - 1) in
    extract new_s (acc @ [m])
  end

let parse_receive s st =
  let open String in
  let c_id = get s 3 in
  if (String.get s 0) == 'f' then
    let snd_c = index_from s 2 ':' in
    let mes = sub s (snd_c + 1) ((length s) - snd_c - 1) in
    let p = (get_c c_id) ^ " failed: " ^ mes in
    {st with print = [red ^ p]}
  else let len_of_uid =
        sub s 6 ((index_from s 6 ':')-6)
        |> int_of_string in
    match (c_id) with
    | 'a' -> {st with print = []}
    | 'b' -> begin
        let len = length s in
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let his = sub s (trd_c + 1) (len - trd_c - 1) in
        {st with print = color cyan (extract his [])}
      end
    | 'c' -> begin
        let snd_c = index_from s 2 ':' in
        let user_num = sub s 6 (snd_c - 6) |> int_of_string in
        if user_num == 0 then
          {st with print = [red ^ "No users online currently."]}
        else let users = sub s (snd_c + 1) ((length s) - snd_c -1) in
          {st with print = color green (extract users [])}
      end
    | 'f' -> begin
        let uid =
          sub s ((index_from s 6 ':') + 1) len_of_uid
          |> int_of_string in
        {
          userid = uid;
          curr_chat = ("lobby", 0);
          chats = [("lobby", 0)];
          print = []
        }
        end
    | 'i' -> begin
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let pub_chats = sub s (trd_c + 1) ((length s) - trd_c - 1) in
        if (extract pub_chats []) <> [] then
          {st with print = color purp (extract pub_chats [])}
        else {st with print = [red ^ "No public chats available currently."]}
      end
    (*Response strings involving <len of chatid>:<chatid>*)
    | others ->
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let trd_comma = index_from s (trd_c + 1) ',' in
        let fth_c = index_from s (trd_c + 1) ':' in
        let chatid = sub s (trd_c + 1) (trd_comma - trd_c - 1)
                     |> int_of_string in
        let info = sub s (fth_c + 1) ((length s) - fth_c - 1) in
        match others with
        | 'h' -> {
          userid = st.userid;
          curr_chat = ("lobby", 0);
          chats = List.remove_assoc info st.chats;
          print = [red ^ "Returning to " ^ purp ^ "lobby" ^ red ^ "..."]
        }
        | 'j' -> begin
          if ((snd st.curr_chat) <> chatid) then
            {st with print = []}
          else {st with print = color blue [info]}
        end
        | 'k' -> {
            userid = st.userid;
            curr_chat = st.curr_chat;
            chats = (info, chatid) :: st.chats;
            print = [green ^ info ^ red ^ " has started a chat with you!"]
          }
(* d, e, g all give the same thing, assuming that the [curr_chatid]
  is automaticially swtiched to that of any newly created chat. *)
        | same -> {
            userid = st.userid;
            curr_chat = (info, chatid);
            chats = (info, chatid) :: st.chats;
            print = [red ^ "Entering chat " ^ purp ^ info ^ red ^ "..."];
        }

(* TODO: Yo James, would you mind work on this?
   [parse_receive] takes in [s], a server repsonse in string, and returns
   a [server_response].

   type server_response = {
     r_userid: int;
     success : bool;
     info : info;

     type state = {
       userid : int;
       curr_chatid : int;
       chats : (string * int) list;
       print: string;
     }
   }

   r_userid is -2 if no userid was returned in response, -1 if response was failure
*)

(*
let parse s =
  let sf_response =
  match String.get s 0 with
    | 's' -> true
    | 'f' -> false
    | _ -> failwith "Invalid server response: s/f" in
  (* This part handles failures *)
  if sf_response = false then
    let len_of_uid =
      String.sub s 6 ((String.index_from s 6 ':')-6)
      |> int_of_string in
    let first_data =
      String.sub s ((String.index_from s 6 ':') + 1) len_of_uid in
    {
      r_userid = -1;
      success = sf_response;
      info = Err_msg (first_data)
    }
    (* This part handles successes *)
  else let cmd_id_response =
    String.get s 3 in
  let len_of_uid =
    String.sub s 6 ((String.index_from s 6 ':')-6)
    |> int_of_string in
  let first_data =
    String.sub s ((String.index_from s 6 ':') + 1) len_of_uid in
  let response_uid =
    first_data
    |> int_of_string in
  let response_info =
    (*this finds the chat_id for functions which have the chatid last.
      may not be helpful

      let find_chatid_as_last =
      let identifier_index = String.rindex s ':' + 1 in
      String.sub s (identifier_index) ((String.length s) - identifier_index) in*)
    match cmd_id_response with
    | 'a' -> ISend_msg ()
               (*TODO: GET_HISTORY. will be the most complicated.*)
    | 'b' -> failwith "unimplemented"
    | 'c' -> let rec username_list str un_left =
               if un_left = 0 then []
               else let index_of_username = (String.rindex str ':') + 1 in
                 let len_of_username = String.length str - index_of_username in
                 let new_str = String.sub str 0 (index_of_username-1) in
                 String.sub str index_of_username len_of_username
                    :: (username_list new_str (un_left-1)) in
      IGet_online_users (username_list s len_of_uid)
    | 'd' -> ICreate_priv_chat ()
    | 'e' -> ICreate_pub_chat ()
    | 'f' -> ICreate_user ()
    | 'g' -> IJoin_chat ()
    | 'h' -> ILeave_chat ()
    | 'i' -> (*Note: edge case of ':' not found will not happen because
               by default there is a lobby public chat - same for 'c' -> the
               client will always be at least 1 user online.
              TODO: Write specs explaining that?*)
      let num_pubchats =
               let num_loc =
                 ((String.index_from s ((String.index s ',') + 1) ',') + 2) in
               String.sub s
                  num_loc
                  ((String.index_from s num_loc ':')-num_loc)
               |> int_of_string in
      let rec chat_list str un_left =
               if un_left = 0 then []
               else let index_of_chatname = (String.rindex str ':') + 1 in
                 let len_of_chatname = String.length str - index_of_chatname in
                 let new_str = String.sub str 0 (index_of_chatname-1) in
                 String.sub str index_of_chatname len_of_chatname
                 :: (chat_list new_str (un_left-1)) in
      IGet_public_chats (chat_list s num_pubchats)
    | _ -> failwith "Invalid server response command id" in
  if sf_response = true && (cmd_id_response <> 'c') then
  {
    r_userid = response_uid;
    success = sf_response;
    info = response_info
  }
  else if sf_response = true && (cmd_id_response == 'c') then
    {
      r_userid = -2;
      success = sf_response;
      info = response_info
    }
  (*might need to add a try-catch depending on client-server stuff*)
  else failwith "Server response error"
*)


    (*
let rec chat st =
  match parse (read_line ()) with
     | End -> ()
     | c ->
  let get_output st = function
    | Create_user s -> create_user s
    | Send_msg s -> send_msg st s
    | Get_public_chats () -> get_public_chats st ()
    | Get_online_users () -> get_online_users st ()
    | Get_curr_chats () -> get_curr_chats st ()
    | Join_chat s -> join_chat st s
    | Change_chat s -> change_chat st s
    | Get_history () -> get_history st ()
    | Create_priv_chat s -> create_private_chat st s
    | Create_pub_chat s -> create_group_chat st s
    | Leave_chat s -> leave_chat st s
    | End -> failwith "should not happen" in
  send (get_output st c);
  let r = receive () in
  (* TODO: Should insert print function here based on [info]*)
  (* TODO: Probably need an [update] function for these. *)
  if r.success then
    let new_st =
      { userid = r.userid;
      (* TODO: Replace these dummy values later. *)
        curr_chatid = 0;
        chats = [];} in
    chat new_st
  else chat st

let main () =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to Camel Chat!\n");
  print_endline "Please initiate your user name.\n";
  print_string  "> ";
  send (create_user (read_line ()));
  let r = receive () in
  let init_st =
    { userid = r.userid;
      (* TODO: Replace the dummy 0 here after learn more. *)
      curr_chatid = 0;
      chats = [];} in
  chat init_st
*)
