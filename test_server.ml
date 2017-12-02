open OUnit2
open Server


let a = "a, 1:0, 6:h,ey:!, 2:15"
let b = "b, 2:10, 1:7"
let c = "c, 7:1234567"
let d = "d, 1:9, 6:js2572"
let e = "e, 1:9, 18::best chat ever!!!"
let f = "f, 6:js2572"
let g = "g, 1:9, 17:best chat ever!!!"
let h = "h, 1:9, 17:best chat ever!!!"
let i = "i, 1:9"

(* let (ra : response) = {userid = 0; cmd = "a"; success = true; info = Nil; chatid = 15} *)


let tests_parse = [
  "a" >:: (fun _ -> assert_equal (input_of_string a) {userid = 0; cmd = Send_msg (15, "h,ey:!")});
  "b" >:: (fun _ -> assert_equal (input_of_string b) {userid = 10; cmd = Get_history 7});
  "c" >:: (fun _ -> assert_equal (input_of_string c) {userid = 1234567; cmd = Get_online_users});
  "d" >:: (fun _ -> assert_equal (input_of_string d) {userid = 9; cmd = Create_priv_chat "js2572"});
  "e" >:: (fun _ -> assert_equal (input_of_string e) {userid = 9; cmd = Create_pub_chat ":best chat ever!!!"});
  "f" >:: (fun _ -> assert_equal (input_of_string f) {userid = -1; cmd = Create_user "js2572"});
  "g" >:: (fun _ -> assert_equal (input_of_string g) {userid = 9; cmd = Join_chat "best chat ever!!!"});
  "h" >:: (fun _ -> assert_equal (input_of_string h) {userid = 9; cmd = Leave_chat "best chat ever!!!"});
  "i" >:: (fun _ -> assert_equal (input_of_string i) {userid = 9; cmd = Get_public_chats});
]

let tests_server = []

let suite = "Server test suite" >::: tests_parse @ tests_server

let _ = run_test_tt_main suite
