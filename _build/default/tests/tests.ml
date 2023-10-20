open OUnit2
open Main

let test = [ ("Sample" >:: fun _ -> assert_equal true true) ]
let suite = "test suite" >::: test
let () = run_test_tt_main suite
