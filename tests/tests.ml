open OUnit2
open Builder

(** Printing functions taken from A2*)

(** [cmp_bag_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent bag-like lists. That means checking that they they contain the
    same elements with the same number of repetitions, though not necessarily in
    the same order. *)
let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let pp_tuple s =
  match s with x, y -> "\"" ^ string_of_int x ^ " " ^ string_of_int y ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

module Test_BuildBoard = Builder.BuildBoard

let valid_points_tests =
  [
    (*Tests for is_corner*)
    ( "top left corner tile" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_corner (0, 0)) );
    ( "bottom left corner tile" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_corner (3, 0)) );
    ( "top right corner tile" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_corner (0, 3)) );
    ( "bottom right corner tile" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_corner (3, 3)) );
    ( "top edge tile" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_corner (0, 1)) );
    ( "left edge tile" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_corner (2, 0)) );
    ( "center tile" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_corner (2, 2)) );
    (*Tests for is_edge*)
    ( "corner tile" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_edge (0, 0)) );
    ( "left edge tile" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_edge (2, 0)) );
    ( "bottom edge tile" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_edge (3, 2)) );
    ( "center tile" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_edge (1, 2)) );
    (*Tests for valid_moves*)
    ( "valid_moves top left corner tile" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple) ~cmp:cmp_bag_like_lists
        [ (0, 1); (1, 0); (1, 1) ]
        (Test_BuildBoard.valid_moves (0, 0)) );
    ( "valid_moves bottom right coner tile" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple) ~cmp:cmp_bag_like_lists
        [ (2, 3); (3, 2); (2, 2) ]
        (Test_BuildBoard.valid_moves (3, 3)) );
    ( "valid_moves top edge tile" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple) ~cmp:cmp_bag_like_lists
        [ (0, 1); (1, 1); (1, 2); (1, 3); (0, 3) ]
        (Test_BuildBoard.valid_moves (0, 2)) );
    ( "valid_moves left edge tile" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple) ~cmp:cmp_bag_like_lists
        [ (0, 0); (0, 1); (1, 1); (2, 1); (2, 0) ]
        (Test_BuildBoard.valid_moves (1, 0)) );
    ( "valid_moves center tile" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple) ~cmp:cmp_bag_like_lists
        [ (1, 1); (1, 2); (1, 3); (2, 1); (3, 1); (3, 2); (3, 3); (2, 3) ]
        (Test_BuildBoard.valid_moves (2, 2)) );
    ( "valid_moves center tile" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple) ~cmp:cmp_bag_like_lists
        [ (0, 1); (0, 2); (0, 3); (1, 3); (2, 3); (2, 2); (2, 1); (1, 1) ]
        (Test_BuildBoard.valid_moves (1, 2)) );
  ]

let b1 = Array.make_matrix 4 4 "a"

let b2 =
  let start = Array.make_matrix 4 4 'a' in
  start.(0).(0) <- 'b';
  start

let b3 =
  let start = Array.make_matrix 4 4 'a' in
  start.(3).(0) <- 'b';
  start.(1).(3) <- 'b';
  start.(2).(2) <- 'b';
  start.(3).(1) <- 'b';
  start

let test_board1 =
  let start = Array.make_matrix 4 4 'x' in
  start.(0).(0) <- 'C';
  start.(0).(1) <- 'A';
  start.(0).(2) <- 'T';
  start.(0).(3) <- 'D';
  start.(1).(0) <- 'W';
  start.(1).(1) <- 'O';
  start.(1).(2) <- 'F';
  start.(1).(3) <- 'O';
  start.(2).(0) <- 'I';
  start.(2).(1) <- 'R';
  start.(2).(2) <- 'D';
  start.(2).(3) <- 'G';
  start.(3).(0) <- 'N';
  start.(3).(1) <- 'O';
  start.(3).(2) <- 'A';
  start.(3).(3) <- 'S';
  start

let () = Test_BuildBoard.print_board b2

let board_tests =
  [
    (*find_chars tests*)
    ( "find_chars board with all the same tiles" >:: fun _ ->
      assert_equal
        [
          (0, 0);
          (0, 1);
          (0, 2);
          (0, 3);
          (1, 0);
          (1, 1);
          (1, 2);
          (1, 3);
          (2, 0);
          (2, 1);
          (2, 2);
          (2, 3);
          (3, 0);
          (3, 1);
          (3, 2);
          (3, 3);
        ]
        (Test_BuildBoard.find_chars "a" b1) );
    ( "find_chars board with only one tile of given char" >:: fun _ ->
      assert_equal ~printer:(pp_list pp_tuple)
        [ (0, 0) ]
        (Test_BuildBoard.find_chars 'b' b2) );
    ( "find_chars board with only multiples tiles of given char" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list pp_tuple)
        [ (3, 0); (1, 3); (2, 2); (3, 1) ]
        (Test_BuildBoard.find_chars 'b' b3) );
    (*is_valid_word tests*)
    ( "is_valid_word 'CAT' test_board1" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_valid_word "CAT" test_board1) );
    ( "is_valid_word 'CTS' test_board1" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_valid_word "CTS" test_board1) );
    ( "is_valid_word 'CAG' test_board1" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_valid_word "CAG" test_board1) );
    ( "is_valid_word 'WORDS' test_board1" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_valid_word "WORDS" test_board1) );
    ( "is_valid_word 'WORDT' test_board1" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_valid_word "WORDT" test_board1) );
    ( "is_valid_word 'CACA' test_board1" >:: fun _ ->
      assert_equal false (Test_BuildBoard.is_valid_word "CACA" test_board1) );
    ( "is_valid_word 'FRAGS' test_board1" >:: fun _ ->
      assert_equal true (Test_BuildBoard.is_valid_word "FRAGS" test_board1) );
  ]

let suite =
  "test suite for builder.ml"
  >::: List.flatten [ valid_points_tests; board_tests ]

let () = run_test_tt_main suite
