open OUnit2
open State
open Data

let empty = {curr_conns = [];
             user_list = [];
             priv_chat_list = [];
             pub_chat_list = [];
             pub_chat_names = [];
             chat_msg = []
            }

let c0 = List.rev [(1, "hi"); (2, "hey"); (1, "sup"); (2, "nm"); (2, "u?"); (1, "same.");
                   (1, ":-)"); (2, "blah"); (1, "blahblah"); (2, "LOL"); (1, "bye")]

let c2 = List.rev [(1, "hi"); (3, "hey"); (4, "sup"); (5, "nm"); (1, "u?"); (3, "same.")]

let h0 = [(2, "hey"); (1, "sup"); (2, "nm"); (2, "u?"); (1, "same.");
                   (1, ":-)"); (2, "blah"); (1, "blahblah"); (2, "LOL"); (1, "bye")]

let s1 = {empty with priv_chat_list = [(0, [1;2]); (1, [3;4])];
                     pub_chat_list = [(2, [1;3;4;5]); (3, [3;5]); (4, [4]); (5, [6])];
                     pub_chat_names = [("chat1", 2); ("chat2", 3); ("chat3", 4); ("chat4", 5)];
                     user_list = [(1, "u1"); (2, "u2"); (3, "u3"); (4, "u4"); (5, "u5"); (6, "u6")];
                     chat_msg = [(0, c0);(2, c2)];
         }

let s2 = {s1 with chat_msg = [(2, (5, "yo") :: c2); (0, c0)]}

let s3 = {s1 with user_list = (7, "u7") :: s1.user_list}

let h2 = [(1, "hi"); (3, "hey"); (4, "sup"); (5, "nm"); (1, "u?"); (3, "same."); (5, "yo")]


let tests = [
  "init" >:: (fun _ -> assert_equal (init_state ()) empty);
  "gc1" >:: (fun _ -> assert_equal (get_chats_of_uid s1 1) [0;2]);
  "gc2" >:: (fun _ -> assert_equal (get_chats_of_uid s1 2) [0]);
  "gc3" >:: (fun _ -> assert_equal (get_chats_of_uid s1 3) [1;2;3]);
  "gc4" >:: (fun _ -> assert_equal (get_chats_of_uid s1 4) [1;2;4]);
  "gc5" >:: (fun _ -> assert_equal (get_chats_of_uid s1 5) [2;3]);
  "gc6" >:: (fun _ -> assert_equal (get_chats_of_uid s1 6) [5]);
  "gc7" >:: (fun _ -> assert_raises (UpdateError "User not found") (fun _ -> get_chats_of_uid s1 7));
  "gce" >:: (fun _ -> assert_raises (UpdateError "User not found") (fun _ -> get_chats_of_uid empty 1));
  "prc" >:: (fun _ -> assert_equal (get_priv_chats s1) ([(0, [1;2]); (1, [3;4])]));
  "puc" >:: (fun _ -> assert_equal (get_pub_chats s1) (["chat4"; "chat3"; "chat2"; "chat1"]));
  "gou" >:: (fun _ -> assert_equal (get_online_users s1) (List.rev ["u1";"u2";"u3";"u4";"u5"; "u6"]));
  "gu0" >:: (fun _ -> assert_equal (get_users_of_chat s1 0) ([1;2]));
  "gu1" >:: (fun _ -> assert_equal (get_users_of_chat s1 1) ([3;4]));
  "gu2" >:: (fun _ -> assert_equal (get_users_of_chat s1 2) ([1;3;4;5]));
  "gu3" >:: (fun _ -> assert_equal (get_users_of_chat s1 3) ([3;5]));
  "gu4" >:: (fun _ -> assert_equal (get_users_of_chat s1 4) ([4]));
  "gu5" >:: (fun _ -> assert_equal (get_users_of_chat s1 5) ([6]));
  "gu6" >:: (fun _ -> assert_raises (UpdateError "Chat not found") (fun _ -> get_users_of_chat s1 6));
  "gh0" >:: (fun _ -> assert_equal (get_history s1 0) h0);
  "gh2" >:: (fun _ -> assert_equal (get_history s1 2) (List.rev c2));
  "ghx" >:: (fun _ -> assert_raises (UpdateError "Chat not found") (fun _ -> get_history s1 6));
  "am" >:: (fun _ -> assert_equal (add_msg s1 5 (2, "yo")) (s2));
  "gh'" >:: (fun _ -> assert_equal (get_history s2 2) h2);
  "au7" >:: (fun _ -> assert_equal (add_user s1 7 "u7") s3);
  "au6" >:: (fun _ -> assert_raises (UpdateError "Username taken") (fun _ -> add_user s1 8 "u6"));
]

let suite = "State test suite" >::: tests

let _ = run_test_tt_main suite
