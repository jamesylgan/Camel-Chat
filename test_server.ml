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

let ra = {userid = 0; cmd = "a"; success = true; info = Nil; chatid = 15}
let sa = "s: a, 1:0"

let rb = {userid = 10; cmd = "b"; success = true; info = ISList ([(10, "hey"); (4, "hi")]); chatid = 15}
let sb = "s: b, 2:10, 2:3:hey2:hi"

let rc = {userid = 1234567; cmd = "c"; success = true; info = SList (["u1"; "u2"; "josh"]); chatid = 5}
let sc = "s: c, 3:2:u12:u24:josh"

let rd = {userid = 9; cmd = "d"; success = true; info = String "js2572"; chatid = 1}
let sd = "s: d, 1:9, 1:1, 6:js2572"

let re = {userid = 9; cmd = "e"; success = true; info = String "group chat"; chatid = 3}
let se = "s: e, 1:9, 1:3, 10:group chat"

let rf = {userid = 1; cmd = "f"; success = true; info = Nil; chatid = 0}
let sf = "s: f, 1:1"

let rg = {userid = 1; cmd = "g"; success = true; info = String "group chat"; chatid = 3}
let sg = "s: g, 1:1, 1:3, 10:group chat"

let rh = {userid = 1; cmd = "h"; success = true; info = String "group chat"; chatid = 3}
let sh = "s: h, 1:1, 1:3, 10:group chat"

let ri = {userid = 1; cmd = "i"; success = true; info = SList ["group chat"; "best chat ever!!!"]; chatid = 3}
let si = "s: i, 1:1, 2:10:group chat17:best chat ever!!!"

let rj = {userid = 1; cmd = "j"; success = true; info = String "hey"; chatid = 3}
let sj = "s: j, 1:1, 1:3, 3:hey"

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
  "ra" >:: (fun _ -> assert_equal (string_of_response ra) sa);
  "rb" >:: (fun _ -> assert_equal (string_of_response rb) sb);
  "rc" >:: (fun _ -> assert_equal (string_of_response rc) sc);
  "rd" >:: (fun _ -> assert_equal (string_of_response rd) sd);
  "re" >:: (fun _ -> assert_equal (string_of_response re) se);
  "rf" >:: (fun _ -> assert_equal (string_of_response rf) sf);
  "rg" >:: (fun _ -> assert_equal (string_of_response rg) sg);
  "rh" >:: (fun _ -> assert_equal (string_of_response rh) sh);
  "ri" >:: (fun _ -> assert_equal (string_of_response ri) si);
  "rj" >:: (fun _ -> assert_equal (string_of_response rj) sj);
]

let tests_server = []

let suite = "Server test suite" >::: tests_parse @ tests_server

let _ = run_test_tt_main suite
