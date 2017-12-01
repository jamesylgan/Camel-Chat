open OUnit2
open Server

let tests = [
]

let suite = "Server test suite" >::: tests

let _ = run_test_tt_main suite
