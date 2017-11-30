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

let tests = [
  "init" >:: (fun _ -> assert_equal (init_state ()) empty);
]

let suite = "State test suite" >::: tests

let _ = run_test_tt_main suite
