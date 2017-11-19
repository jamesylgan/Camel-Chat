type info =
  | Str of string
  | Str_lst of string list

type command =
  | Create_user of string
  | Send_msg of string
  | Get_public_chats of unit
  | Get_online_users of unit
  | Get_curr_chats of unit
  | Join_chat of string
  | Change_chat of string
  | Get_history of unit
  | Create_private_chat of string
  | Create_group_chat of string
  | Leave_chat of string

type state = {
  userid : int;
  curr_chatid : int;
  chats : int list
}

type server_response = {
  userid: int;
  success : bool;
  info : info;
}

type client_output = {
  userid : int;
  cmd: command;
}

let parse s = failwith "Unimplemented"

let create_user s =
  {
    (* A dummy value for userid *)
   userid = -11111111;
   cmd = Create_user s;
  }

let send_msg (st: state) s =
  {
    userid = st.userid;
    cmd = Send_msg s;
  }

let get_public_chats (st: state) () =
  {
    userid = st.userid;
    cmd = Get_public_chats ();
  }

let get_online_users (st: state) () =
  {
    userid = st.userid;
    cmd = Get_online_users ();
  }

let get_curr_chats (st: state) () =
  {
    userid = st.userid;
    cmd = Get_curr_chats ();
  }

let join_chat st s = failwith "Unimplemented"

let change_chat st s = failwith "Unimplemented"

let get_history st () = failwith "Unimplemented"

let create_private_chat st s  = failwith "Unimplemented"

let create_group_chat st s = failwith "Unimplemented"

let leave_chat st s = failwith "Unimplemented"

let send output = ()

let receive () = failwith "Unimplemented"

let rec chat st =
  (* TODO: Should we have some kind of quit command that quits like
     match parse (read_line ()) with
     | Quit _ -> ()
     | c -> rest of the function
     or are we just use the global lobby for this ?
  *)
  let get_output st = function
    | Create_user s -> create_user s
    | Send_msg s -> send_msg st s
    | Get_public_chats () -> get_public_chats st ()
    | Get_online_users () -> get_online_users st ()
    | Get_curr_chats () -> get_curr_chats st ()
    | Join_chat s -> join_chat st s
    | Change_chat s -> change_chat st s
    | Get_history () -> get_history st ()
    | Create_private_chat s -> create_private_chat st s
    | Create_group_chat s -> create_group_chat st s
    | Leave_chat s -> leave_chat st s in
  send (get_output st (parse (read_line ())));
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
