(*
type info =
  | ICreate_user of unit
  | ISend_msg of unit
  | IGet_public_chats of string list
  | IGet_online_users of string list
  | IGet_curr_chats of string list
      (*TODO: This is handled client-side, should it really be in info?*)
  | IJoin_chat of unit
  | IChange_chat of unit
  | IGet_history of string list
  | ICreate_priv_chat of unit
  | ICreate_pub_chat of unit
  | ILeave_chat of unit
  | Err_msg of string

type command =
  | Create_user of string
  | Send_msg of string
  | Get_public_chats of unit
  | Get_online_users of unit
  | Get_curr_chats of unit
  | Join_chat of string
  | Change_chat of string
  | Get_history of int
  | Create_priv_chat of string
  | Create_pub_chat of string
  | Leave_chat of string
  | Quit

  type server_response = {
    r_userid: int;
    success : bool;
    info : info;
  }
*)

type state = {
  userid : int;
  curr_chatid : int;
  chats : int list;
  print: string list;
}

let parse_send s st =
  let open String in
  let uid = ", " ^ (st.userid |> string_of_int |> length |> string_of_int)
            ^ ":" ^ (st.userid |> string_of_int) in
  let chatid = ", " ^ (st.curr_chatid |> string_of_int |> length |>
                       string_of_int)
               ^ ":" ^ (st.curr_chatid |> string_of_int) in
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

(* A helper function for [parse_receive] that deals with the [get_history]
 * response string specifically. *)
let rec extract_his s acc =
  let open String in
  match s with
  | "" -> acc
  | s -> begin
    let c_loc = index_from s 0 ':' in
    let len = (sub s 0 c_loc) |> int_of_string in
    let m = sub s (c_loc+1) len in
    let new_s = sub s (c_loc + len + 1) ((length s) - c_loc - len - 1) in
    extract_his new_s (acc @ [m])
  end

let parse_receive s st =
  let open String in
  if (String.get s 0) == 'f' then
    {st with print = ["Sorry your command was rejected by the server."]}
  else let len_of_uid =
        sub s 6 ((index_from s 6 ':')-6)
        |> int_of_string in
    match (get s 3) with
    | 'a' -> st
    | 'b' -> begin
        let len = length s in
        let snd_c = index_from s 2 ':' in
        let trd_c = index_from s (snd_c + 1) ':' in
        let fth_c = index_from s (trd_c + 1) ':' in
        let his = sub s (fth_c + 1) (len - fth_c - 1) in
        {st with print = (extract_his his [])}
      end
    | 'c' -> begin
        let rec username_list str un_left =
          if un_left = 0 then []
          else let index_of_username = (rindex str ':') + 1 in
            let len_of_username = length str - index_of_username in
            let new_str = sub str 0 (index_of_username-1) in
            sub str index_of_username len_of_username
            :: (username_list new_str (un_left-1)) in
        let p = username_list s len_of_uid in
        {st with print = p}
      end
    | 'f' -> begin
        let uid =
          sub s ((index_from s 6 ':') + 1) len_of_uid
          |> int_of_string in
          {st with userid = uid}
        end
    | 'i' -> begin
        let num_pubchats =
          let num_loc =
            ((index_from s ((String.index s ',') + 1) ',') + 2) in
          sub s num_loc
            ((index_from s num_loc ':')-num_loc)
          |> int_of_string in
        let rec chat_list str un_left =
          if un_left = 0 then []
          else let index_of_chatname = (rindex str ':') + 1 in
            let len_of_chatname = length str - index_of_chatname in
            let new_str = sub str 0 (index_of_chatname-1) in
            sub str index_of_chatname len_of_chatname
            :: (chat_list new_str (un_left-1)) in
        let p = chat_list s num_pubchats in
        {st with print = p}
      end
  (*Response strings involving <len of chatid>:<chatid>*)
    | o ->
      let last_c = (rindex s ':') in
      let chatid = sub s (last_c + 1) ((length s)-last_c-1)
                    |> int_of_string in
      match o with
      | 'h' -> failwith "implement after adding chat names to response str"
(* d, e, g all give the same thing, assuming that the [curr_chatid]
  is automaticially swtiched to that of any newly created chat. *)
      | same -> {
          userid = st.userid;
          curr_chatid = chatid;
          chats = chatid :: st.chats;
          print = [""];
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
