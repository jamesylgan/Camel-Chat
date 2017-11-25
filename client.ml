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
  | Get_history of unit
  | Create_priv_chat of string
  | Create_pub_chat of string
  | Leave_chat of string
  | Quit

type state = {
  userid : int;
  curr_chatid : int;
  chats : (string * int) list;
  print: string;
}

type server_response = {
  r_userid: int;
  success : bool;
  info : info;
}

let parse_send s st =
  let open String in
  let uid = ", " ^ (st.userid |> string_of_int |> length |> string_of_int)
            ^ ":" ^ (st.userid |> string_of_int) in
  let chatid = ", " ^ (st.curr_chatid |> string_of_int |> length |>
                       string_of_int)
               ^ ":" ^ (st.curr_chatid |> string_of_int) in
  let open Str in
  match s with
  (* Does not include [Help], [Create_user], and [Quit] which are
     managed in [View.ml]. *)
  | "#history" -> "b" ^ uid ^ chatid
  | "#users" -> "c" ^ uid
  | "#mychats" -> "j" ^ uid
  | "#pubchats" -> "i" ^ uid
  | s -> begin
      let priv_chat = regexp "#chatwith \\(.+\\)" in
      let pub_chat = regexp "#makechat \\(.+\\)" in
      let join_chat = regexp "#join \\(.+\\)" in
      let leave_chat = regexp "#leave \\(.+\\)" in
      let change_chat = regexp "#goto \\(.+\\)" in
      if string_match priv_chat s 0
      then "d" ^ uid ^ ", " ^ ((length s)-10 |> string_of_int) ^
           ":" ^ ((length s)-10 |> sub s 10)
      else if string_match pub_chat s 0
      then "e" ^ uid ^ ", " ^ ((length s)-10 |> string_of_int) ^
           ":" ^ ((length s)-10 |> sub s 10)
      else if string_match join_chat s 0
      then "g" ^ uid ^ ", " ^ ((length s)-6 |> string_of_int) ^
           ":" ^ ((length s)-6 |> sub s 6)
      else if string_match leave_chat s 0
      then "h" ^ uid ^ ", " ^ ((length s)-7 |> string_of_int) ^
           ":" ^ ((length s)-7 |> sub s 7)
      else if string_match change_chat s 0
      then "k" ^ uid ^ ", " ^ ((length s)-6 |> string_of_int) ^
           ":" ^ ((length s)-6 |> sub s 6)
      else "a" ^ uid ^ ", " ^ (s |> length |> string_of_int) ^
           ":" ^ s ^ chatid
    end

(* TODO: Yo James, would you mind work on this?
   [parse_receive] takes in [s], a server repsonse in string, and returns
   a [server_response].

   type server_response = {
     r_userid: int;
     success : bool;
     info : info;
   }

   r_userid is -2 if no userid was returned in response, -1 if response was failure
*)
let receive s =
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

let send output = ()

let receive = failwith "unimplemented, not sure how it will work"
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
