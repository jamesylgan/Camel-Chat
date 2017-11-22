open Data
type state = {
  curr_conns: (int, int) ListDict.t;
  user_list: (int, int) ListDict.t;
  priv_chat_list: (int, int) ListDict.t;
  pub_chat_list:(int, int) ListDict.t;
  pub_chat_names: (int, int) ListDict.t;
  chat_msg: (int, int) ListDict.t;
}

let init_state = failwith "Unimplemented"

let get_chats_of_uid = failwith "Unimplemented"

let get_conns_of_chat = failwith "Unimplemented"

let get_priv_chats = failwith "Unimplemented"

let get_online_users = failwith "Unimplemented"

let get_pub_chats = failwith "Unimplemented"

let get_users_of_chat = failwith "Unimplemented"

let get_history = failwith "Unimplemented"

let add_msg = failwith "Unimplemented"

let add_user = failwith "Unimplemented"

let add_conn = failwith "Unimplemented"

let add_pub_chat = failwith "Unimplemented"

let add_priv_chat = failwith "Unimplemented"

let add_user_to_pub_chat = failwith "Unimplemented"

let get_username = failwith "Unimplemented"

let get_chatid = failwith "Unimplemented"

let remove_user = failwith "Unimplemented"
