open OUnit2
open Builder
open Data_structures
module Test_BuildBoard = Builder.BuildBoard
module TestDict = Data_structures.Dictionary

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

let pp_tuple (s : Test_BuildBoard.location) =
  match s with
  | Loc (x, y) -> "\"" ^ string_of_int x ^ " " ^ string_of_int y ^ "\""

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

let rec list_of_loc_list (loc_list : Test_BuildBoard.location list) acc =
  match loc_list with
  | [] -> List.rev acc
  | Loc (x, y) :: t -> list_of_loc_list t ((x, y) :: acc)

let rec loc_list_of_list lst acc : Test_BuildBoard.location list =
  match lst with
  | [] -> List.rev acc
  | (x, y) :: t -> loc_list_of_list t (Loc (x, y) :: acc)

(*
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
  ]*)

let test_valid_moves name expected_output (x, y) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists
    (loc_list_of_list expected_output [])
    (Test_BuildBoard.valid_moves (Loc (x, y)))

let test_valid_next_tile name expected_output (x1, y1) (x2, y2) =
  name >:: fun _ ->
  assert_equal expected_output
    (Test_BuildBoard.is_valid_next_tile (Loc (x1, y1)) (Loc (x2, y2)))

let points_tests =
  [
    (*valid_moves tests*)
    test_valid_moves "valid_moves top left corner tile"
      [ (0, 1); (1, 0); (1, 1) ]
      (0, 0);
    test_valid_moves "valid_moves bottom right coner tile"
      [ (2, 3); (3, 2); (2, 2) ]
      (3, 3);
    test_valid_moves "valid_moves top edge tile"
      [ (0, 1); (1, 1); (1, 2); (1, 3); (0, 3) ]
      (0, 2);
    test_valid_moves "valid_moves left edge tile"
      [ (0, 0); (0, 1); (1, 1); (2, 1); (2, 0) ]
      (1, 0);
    test_valid_moves "valid_moves center tile (2, 2)"
      [ (1, 1); (1, 2); (1, 3); (2, 1); (3, 1); (3, 2); (3, 3); (2, 3) ]
      (2, 2);
    test_valid_moves "valid_moves center tile (1, 2)"
      [ (0, 1); (0, 2); (0, 3); (1, 3); (2, 3); (2, 2); (2, 1); (1, 1) ]
      (1, 2);
    (*test is_valid_next_tile*)
    test_valid_next_tile "horizontally adjacent tiles" true (0, 0) (0, 1);
    test_valid_next_tile "vertically adjacent tiles" true (0, 0) (1, 0);
    test_valid_next_tile "diagonally adjacent tiles" true (0, 0) (1, 1);
    test_valid_next_tile "non adjacent tiles; corner tiles" false (0, 0) (3, 3);
    test_valid_next_tile "non adjacent tiles; one tile away" false (0, 0) (0, 2);
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

(*typical board*)
let test_board1 =
  let start = Array.make_matrix 4 4 "x" in
  start.(0).(0) <- "C";
  start.(0).(1) <- "A";
  start.(0).(2) <- "T";
  start.(0).(3) <- "D";
  start.(1).(0) <- "W";
  start.(1).(1) <- "O";
  start.(1).(2) <- "F";
  start.(1).(3) <- "O";
  start.(2).(0) <- "I";
  start.(2).(1) <- "R";
  start.(2).(2) <- "D";
  start.(2).(3) <- "G";
  start.(3).(0) <- "N";
  start.(3).(1) <- "O";
  start.(3).(2) <- "A";
  start.(3).(3) <- "S";
  start

(*board with no solutions*)
let test_board2 =
  let start = Array.make_matrix 4 4 "x" in
  start.(0).(0) <- "@";
  start.(0).(1) <- "@";
  start.(0).(2) <- "@";
  start.(0).(3) <- "@";
  start.(1).(0) <- "@";
  start.(1).(1) <- "@";
  start.(1).(2) <- "@";
  start.(1).(3) <- "@";
  start.(2).(0) <- "@";
  start.(2).(1) <- "@";
  start.(2).(2) <- "@";
  start.(2).(3) <- "@";
  start.(3).(0) <- "@";
  start.(3).(1) <- "@";
  start.(3).(2) <- "@";
  start.(3).(3) <- "@";
  start

(*board with multiple words from same path*)
let test_board3 =
  let start = Array.make_matrix 4 4 "x" in
  start.(0).(0) <- "C";
  start.(0).(1) <- "A";
  start.(0).(2) <- "S";
  start.(0).(3) <- "T";
  start.(1).(0) <- "@";
  start.(1).(1) <- "@";
  start.(1).(2) <- "@";
  start.(1).(3) <- "A";
  start.(2).(0) <- "@";
  start.(2).(1) <- "@";
  start.(2).(2) <- "@";
  start.(2).(3) <- "W";
  start.(3).(0) <- "@";
  start.(3).(1) <- "S";
  start.(3).(2) <- "Y";
  start.(3).(3) <- "A";
  start

let () = Test_BuildBoard.print_board test_board3

let mini_board =
  let start = Array.make_matrix 4 4 "x" in
  start.(0).(0) <- "C";
  start.(0).(1) <- "A";
  start.(1).(0) <- "T";
  start.(1).(1) <- "E";
  start

let empty_hashtable = Hashtbl.create 10
let () = Test_BuildBoard.solve test_board1 empty_hashtable

(*let () = Test_BuildBoard.traverse mini_board 0 0 "" [] empty_hashtable*)
let () = Test_BuildBoard.print_board test_board1

let () =
  print_endline (pp_list pp_string (Test_BuildBoard.solutions empty_hashtable))

(*Helper functions for tests*)
let test_is_valid_word name expected_output lst board =
  name >:: fun _ ->
  assert_equal expected_output
    (Test_BuildBoard.is_valid_word2 (loc_list_of_list lst []) board)

let test_solve name expected_output input =
  name >:: fun _ ->
  let hashtable = Hashtbl.create 10 in
  Test_BuildBoard.solve input hashtable;
  assert_equal ~printer:(pp_list pp_string) expected_output
    (Test_BuildBoard.longest_words (Test_BuildBoard.solutions hashtable) 10)

let board_tests =
  [
    (*find_chars tests*)
    (*
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
        [ Loc (0, 0) ]
        (Test_BuildBoard.find_chars 'b' b2) );
    ( "find_chars board with only multiples tiles of given char" >:: fun _ ->
      assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list pp_tuple)
        [ (3, 0); (1, 3); (2, 2); (3, 1) ]
        (Test_BuildBoard.find_chars 'b' b3) );*)
    (*is_valid_word tests*)
    test_is_valid_word "is_valid_word '' test_board1" false [] test_board1;
    test_is_valid_word "is_valid_word 'A' test_board1" false
      [ (0, 1) ]
      test_board1;
    test_is_valid_word "is_valid_word 'A' test_board1" false
      [ (0, 1) ]
      test_board1;
    test_is_valid_word "is_valid_word 'AT' test_board1" false
      [ (0, 1); (0, 2) ]
      test_board1;
    test_is_valid_word "is_valid_word 'CAT' test_board1" true
      [ (0, 0); (0, 1); (0, 2) ]
      test_board1;
    test_is_valid_word "is_valid_word 'CTS' test_board1" false
      [ (0, 0); (0, 2); (3, 3) ]
      test_board1;
    test_is_valid_word "is_valid_word 'CAG' test_board1" false
      [ (0, 0); (0, 1); (2, 3) ]
      test_board1;
    test_is_valid_word "is_valid_word 'WORDS' test_board1" true
      [ (1, 0); (1, 1); (2, 1); (2, 2); (3, 3) ]
      test_board1;
    test_is_valid_word "is_valid_word 'WORDT' test_board1" false
      [ (1, 0); (1, 1); (2, 1); (2, 2); (0, 3) ]
      test_board1;
    test_is_valid_word "is_valid_word 'CACA' test_board1" false
      [ (0, 0); (0, 1); (0, 0); (0, 0) ]
      test_board1;
    test_is_valid_word "is_valid_word 'WINI' test_board1" false
      [ (0, 1); (0, 2); (0, 3); (0, 2) ]
      test_board1;
    test_is_valid_word "is_valid_word 'FRAGS' test_board1" true
      [ (1, 2); (2, 1); (3, 2); (2, 3); (3, 3) ]
      test_board1;
    test_is_valid_word "is_valid_word 'CAFAC' test_board1" false
      [ (0, 0); (0, 1); (1, 2); (0, 1); (0, 0) ]
      test_board1;
    test_is_valid_word "is_valid_word 'INROADS' test_board1" true
      [ (2, 0); (3, 0); (2, 1); (3, 1); (3, 2); (2, 2); (3, 3) ]
      test_board1;
    test_is_valid_word "is_valid_word test_board2" false
      [ (0, 0); (0, 1); (1, 2); (0, 1); (0, 0) ]
      test_board2;
    test_is_valid_word "is_valid_word 'CASTAWAYS' test_board3" true
      [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 3); (2, 3); (3, 3); (3, 2); (3, 1) ]
      test_board3;
    test_is_valid_word "is_valid_word 'CASTAWAY' test_board3" true
      [ (0, 0); (0, 1); (0, 2); (0, 3); (1, 3); (2, 3); (3, 3); (3, 2) ]
      test_board3;
    test_is_valid_word "is_valid_word 'CATS' test_board3" false
      [ (0, 0); (0, 1); (0, 3); (0, 2) ]
      test_board3;
    (*solve tests*)
    test_solve "Test_BuildBord.solve test_board1"
      [
        "INROADS";
        "INROAD";
        "CORDON";
        "CORDS";
        "CODON";
        "SAROD";
        "SAROD";
        "ACORN";
        "TOGAS";
        "GARNI";
      ]
      test_board1;
    test_solve "Test_BuildBord.solve test_board2" [] test_board2;
    test_solve "Test_BuildBord.solve test_board3"
      [
        "CASTAWAYS";
        "CASTAWAY";
        "AWAYS";
        "CASA";
        "WAYS";
        "WATS";
        "TAWA";
        "WAST";
        "AWAY";
        "STAW";
      ]
      test_board3;
  ]

let suite =
  "test suite for builder.ml" >::: List.flatten [ points_tests (*board_tests*) ]

let () = run_test_tt_main suite
