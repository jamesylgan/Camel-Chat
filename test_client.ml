open OUnit2
open Client

let red = "\027[31m"
let blue = "\027[34m"
let green = "\027[32m"
let purp = "\027[35m"
let cyan = "\027[36m"

let color c lst = List.map (fun x -> c ^ x) lst

let st0 = init_state ()
let st1 = {
  userid = 1;
  curr_chat = ("lobby", 0);
  chats = [("lobby", 0)];
  print = [];
}
let st2 = {
  userid = 10;
  curr_chat = ("cs3110", 42);
  chats = [("cs3110", 42); ("lobby", 0)];
  print = ["default"];
}
let st3 = {
  userid = 1;
  curr_chat = ("cs3110", 42);
  chats = [("cs3110", 42); ("lobby", 0)];
  print = [red ^ "Entering chat " ^ purp ^ "cs3110" ^ red ^ "..."];
}
let st4 = {
  userid = 10;
  curr_chat = ("cornell", 15);
  chats = [("cornell", 15); ("cs3110", 42); ("lobby", 0)];
  print = [red ^ "Entering chat " ^ purp ^ "cornell" ^ red ^ "..."];
}
let st5 = {
  userid = 1;
  curr_chat = ("lobby", 0);
  chats = [("Josh", 2); ("lobby", 0)];
  print = ["\027[32mJosh\027[31m has started a chat with you."];
}
let st6 = {
  userid = 10;
  curr_chat = ("cs3110", 42);
  chats = [("cs3110", 42); ("lobby", 0)];
  print = [];
}

let parse_create_user_tests = [
  "Create User1" >:: (fun _ -> assert_equal "f, 6:Turing" (parse_create_user "Turing"));
  "Create User2" >:: (fun _ -> assert_equal "f, 9:Jack123:)" (parse_create_user "Jack123:)"));
]

let parse_send_tests = [
  (* Test cases on command "get history". *)
  "Get history1" >:: (fun _ -> assert_equal "b, 1:1, 1:0" (parse_send "#history" st1));
  "Get history2" >:: (fun _ -> assert_equal "b, 2:10, 2:42" (parse_send "#history" st2));
  (* Test cases on command "create private chat". *)
  "Create priv chat1" >:: (fun _ -> assert_equal "d, 1:1, 8:Clarkson" (parse_send "#chatwith Clarkson" st1));
  "Create priv chat2" >:: (fun _ -> assert_equal "d, 2:10, 7:Hopcoft" (parse_send "#chatwith Hopcoft" st2));
  (* Test cases on command "create public chat". *)
  "Create pub chat1" >:: (fun _ -> assert_equal "e, 1:1, 9:class2019" (parse_send "#makechat class2019" st1));
  "Create pub chat2" >:: (fun _ -> assert_equal "e, 2:10, 7:cornell" (parse_send "#makechat cornell" st2));
  (* Test cases on command "join chat". *)
  "Join chat1" >:: (fun _ -> assert_equal "g, 1:1, 9:class2019" (parse_send "#join class2019" st1));
  "Join chat2" >:: (fun _ -> assert_equal "g, 2:10, 7:cornell" (parse_send "#join cornell" st2));
  (* Test cases on command "leave chat". *)
  "Leave chat1" >:: (fun _ -> assert_equal "h, 1:1, 9:class2019" (parse_send "#leave class2019" st1));
  "Leave chat2" >:: (fun _ -> assert_equal "h, 2:10, 7:cornell" (parse_send "#leave cornell" st2));
  (* Test cases on command "get online users". *)
  "Get online users1" >:: (fun _ -> assert_equal  "c, 1:1"  (parse_send "#users" st1));
  "Get online users2" >:: (fun _ -> assert_equal  "c, 2:10"  (parse_send "#users" st2));
  (* Test cases on command "get public chats". *)
  "Get pub chat1" >:: (fun _ -> assert_equal  "i, 1:1"  (parse_send "#pubchats" st1));
  "Get pub chat2" >:: (fun _ -> assert_equal  "i, 2:10"  (parse_send "#pubchats" st2));
  (* Test case on command "send message". *)
  "Send msg1" >:: (fun _ -> assert_equal "a, 1:1, 25:How's your history class?, 1:0"
                             (parse_send "How's your history class?" st1));
  "Send msg2" >:: (fun _ -> assert_equal "a, 2:10, 14:Time to leave!, 2:42"
                             (parse_send "Time to leave!" st2));
]

let parse_receive_tests = [
  (* Test cases on response "send_msg". *)
  "SEND_MSG1" >:: (fun _ -> assert_equal st1 (parse_receive "s: a, 1:0" st1));
  "SEND_MSG2" >:: (fun _ -> assert_equal {st2 with print = []} (parse_receive "s: a, 2:10" st2));
  (* Test cases on response "get_history". *)
  "GET_HISTORY0" >:: (fun _ -> assert_equal {st1 with print = []}
                         (parse_receive "s: b, 1:1, 0:" st1));
  "GET_HISTORY1" >:: (fun _ -> assert_equal {st1 with print = color cyan ["Tim:Hello"]}
                         (parse_receive "s: b, 1:1, 1:9:Tim:Hello" st1));
  "GET_HISTORY2" >:: (fun _ -> assert_equal {st2 with print = color cyan ["Tim:Hello"; "Jack:Hi"; "Tim:How's your day?"]}
                         (parse_receive "s: b, 2:10, 3:9:Tim:Hello7:Jack:Hi19:Tim:How's your day?" st2));
  (* Test cases on response "get online users". *)
  "GET_ONLINE_USERS0" >:: (fun _ -> assert_equal {st1 with print = [red^"No users online currently."]}
                              (parse_receive "s: c, 0:" st1));
  "GET_ONLINE_USERS1" >:: (fun _ -> assert_equal {st1 with print = color green ["Clarkson"]}
                              (parse_receive "s: c, 1:8:Clarkson" st1));
  "GET_ONLINE_USERS2" >:: (fun _ -> assert_equal {st2 with print = color green ["Tim"; "Jack"; "Clarkson"]}
                              (parse_receive "s: c, 3:3:Tim4:Jack8:Clarkson" st2));
  (* Test cases on response "create_priv_chat". *)
  "CREATE_PRIV_CHAT1" >:: (fun _ -> assert_equal st3 (parse_receive "s: d, 1:1, 2:42, 6:cs3110" st1));
  "CREATE_PRIV_CHAT2" >:: (fun _ -> assert_equal st4 (parse_receive "s: d, 2:10, 2:15, 7:cornell" st2));
  (* Test cases on response "create_pub_chat". *)
  "CREATE_PUB_CHAT1" >:: (fun _ -> assert_equal st3 (parse_receive "s: e, 1:1, 2:42, 6:cs3110" st1));
  "CREATE_PUB_CHAT2" >:: (fun _ -> assert_equal st4 (parse_receive "s: e, 2:10, 2:15, 7:cornell" st2));
  (* Test case on response "create_user". *)
  "CREATE_USER0" >:: (fun _ -> assert_equal {st1 with print = []}
                         (parse_receive "s: f, 1:1" st0));
  (* Test cases on response "Join_chat". *)
  "JOIN_CHAT1" >:: (fun _ -> assert_equal st3 (parse_receive "s: g, 1:1, 2:42, 6:cs3110" st1));
  "JOIN_CHAT2" >:: (fun _ -> assert_equal st4 (parse_receive "s: g, 2:10, 2:15, 7:cornell" st2));
  (* Test case on response "leave_chat". *)
  "LEAVE_CHAT0" >:: (fun _ -> assert_equal {st1 with print = [red ^ "Returning to " ^ purp ^ "lobby" ^ red ^ "..."]}
                        (parse_receive "s: h, 1:1, 2:42, 6:cs3110" st3));
(* Test cases on response "chat_notification". *)
  "CHAT_NOTIFICATION1" >:: (fun _ -> assert_equal st5
                               (parse_receive "s: k, 1:1, 1:2, 4:Josh, 29: has started a chat with you." st1));
  "CHAT_NOTIFICATION2" >:: (fun _ -> assert_equal st6
                               (parse_receive "s: k, 2:10, 1:2, 6:Jojo:), 21: has joined the chat." st2));
  (* Test cases on response "get_pub_chat". *)
  "GET_PUB_CHAT0" >:: (fun _ -> assert_equal {st1 with print = [red^"No public chats available currently."]}
                         (parse_receive "s: i, 1:1, 0:" st1));
  "GET_PUB_CHAT1" >:: (fun _ -> assert_equal {st1 with print = color purp ["cs3110"]}
                         (parse_receive "s: i, 2:10, 1:6:cs3110" st1));
  "GET_PUB_CHAT2" >:: (fun _ -> assert_equal {st2 with print = color purp ["cs3110"; "cornell"; "cs:)"]}
                          (parse_receive "s: i, 2:10, 3:6:cs31107:cornell4:cs:)" st2));
(* Test cases on response "receive_msg". *)
  "RECEIVE_MSG1" >:: (fun _ -> assert_equal {st1 with print = []}
                         (parse_receive "s: j, 1:1, 2:42, 14:Hi everyone :D" st1));
  "RECEIVE_MSG2" >:: (fun _ -> assert_equal {st2 with print = color blue ["Hi everyone :D"]}
                         (parse_receive "s: j, 1:1, 2:42, 14:Hi everyone :D" st2));
(* Test cases on response "failure". *)
  "FAILURE1" >:: (fun _ -> assert_equal {st1 with print = [red^"Chat name already taken :("]}
                     (parse_receive "f: e, 26:Chat name already taken :(" st1));
  "FAILURE2" >:: (fun _ -> assert_equal {st2 with print = [red^"The intended chat is not available."]}
                     (parse_receive "f: g, 35:The intended chat is not available." st2));
]

let suite = "Client test suite" >::: List.flatten [parse_create_user_tests;
                                                   parse_send_tests;
                                                  parse_receive_tests]

let _ = run_test_tt_main suite
