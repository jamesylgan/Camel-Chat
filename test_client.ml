open OUnit2
open Client

let tests = [
]

let suite = "Client test suite" >::: tests

let _ = run_test_tt_main suite
