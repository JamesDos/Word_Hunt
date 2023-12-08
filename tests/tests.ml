open OUnit2
open Builder
open Data_structures
module Test_BuildBoard = Builder.BuildBoard
module TestDict = Data_structures.Dictionary
module TestTrie = Data_structures.Trie

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

(**[test_valid_pos] is a helper function to test [Builder.is_valid_pos]*)
let test_valid_pos name expected_output (x, y) =
  name >:: fun _ ->
  assert_equal expected_output (Test_BuildBoard.is_valid_pos (Loc (x, y)))

(**[test_valid_moves] is a helper function to test [Builder.valid_moves]*)
let test_valid_moves name expected_output (x, y) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_bag_like_lists
    (loc_list_of_list expected_output [])
    (Test_BuildBoard.valid_moves (Loc (x, y)))

(**[test_valid_next_tile] is a helper function to test [Builder.is_valid_next_tile]*)
let test_valid_next_tile name expected_output (x1, y1) (x2, y2) =
  name >:: fun _ ->
  assert_equal expected_output
    (Test_BuildBoard.is_valid_next_tile (Loc (x1, y1)) (Loc (x2, y2)))

let points_tests =
  [
    (*is_valid_pos tests*)
    test_valid_pos "valid pos corner (0, 0)" true (0, 0);
    test_valid_pos "valid pos edge (1, 3)" true (1, 3);
    test_valid_pos "valid pos center tile (2, 1)" true (2, 1);
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

let t1 =
  let root = TestTrie.create_node () in
  TestTrie.insert_list_of_words root
    [ "apple"; "app"; "orange"; "a"; "castaways" ];
  root

let t2 = TestDict.trie

let test_trie name expected_output word trie =
  name >:: fun _ ->
  assert_equal expected_output
    (TestTrie.search_word trie (TestTrie.to_char_list word))

let trie_tests =
  [
    (*search_word t1 tests*)
    test_trie "apple" true "apple" t1;
    test_trie "app" true "app" t1;
    test_trie "orange" true "orange" t1;
    test_trie "a" true "a" t1;
    test_trie "castaways" true "castaways" t1;
    test_trie "cast; prefix subword of castaways" false "cast" t1;
    test_trie "away: suffix subword of castaways" false "away" t1;
    test_trie "xhxsiz; not letters in trie" false "xhxsiz" t1;
    test_trie "oeragn; same letters but different order" false "oeragn" t1;
    test_trie "appls; contains same prefix as apple" false "appls" t1;
    test_trie "ap; contains same prefix as app " false "ap" t1;
    test_trie "empty string" false "" t1;
    (*search_word TestDict.trie tests*)
    test_trie "a t2" false "a" t2;
    test_trie "A t2" false "A" t2;
    test_trie "and t2 (only accepts uppercase letters)" false "and" t2;
    test_trie "APPLE t2" true "APPLE" t2;
    test_trie "APP t2" true "APP" t2;
    test_trie "ORANGE t2" true "ORANGE" t2;
    test_trie "HIPPOPOTAMUSES t2" true "HIPPOPOTAMUSES" t2;
    test_trie "SAJFKDLSFD t2" false "SAJFKDLSFD" t2;
  ]

(**[test_is_word] is a helper function to test Dictionary.is_word*)
let test_is_word name expected_output word =
  name >:: fun _ -> assert_equal expected_output (TestDict.is_word word)

let dict_tests =
  [
    (*is_word tests*)
    test_is_word "is_word valid word a" true "a";
    test_is_word "is_word valid word empty string" false "";
    test_is_word "is_word valid word fduafojal" false "fduafojal";
    test_is_word "is_word valid word HIPPOPOTAMUSES" true "hippopotamuses";
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
  let start = Array.make_matrix 4 4 "@" in
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

let mega_board = Array.make_matrix 5 5 "x"

(*prints board [b] and its solutions*)
let print_board_solutions b =
  let empty_hashtable = Hashtbl.create 10 in
  let () =
    Test_BuildBoard.solve b empty_hashtable;
    Test_BuildBoard.print_board b
  in
  print_endline (pp_list pp_string (Test_BuildBoard.solutions empty_hashtable))

(**[test_make_word] is a helper function to test Builder.make_word*)
let test_make_word name expected_output lst board =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output
    (Test_BuildBoard.make_word (loc_list_of_list lst []) board)

(**[test_is_valid_words] is a helper function to test Builder.is_valid_word*)
let test_is_valid_word name expected_output lst board =
  name >:: fun _ ->
  assert_equal expected_output
    (Test_BuildBoard.is_valid_word (loc_list_of_list lst []) board)

(**[test_solve] is a helper function to test Builder.solve*)
let test_solve name expected_output input =
  name >:: fun _ ->
  let hashtable = Hashtbl.create 10 in
  Test_BuildBoard.solve input hashtable;
  assert_equal ~cmp:cmp_bag_like_lists ~printer:(pp_list pp_string)
    expected_output
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
    (*make_word tests*)
    test_make_word "make word empty loc lst" "" [] test_board1;
    test_make_word "make word singeton loc lst" "C" [ (0, 0) ] test_board1;
    test_make_word "make word CAT test_board1" "CAT"
      [ (0, 0); (0, 1); (0, 2) ]
      test_board1;
    test_make_word "make word ACT test_board1" "ACT"
      [ (0, 1); (0, 0); (0, 2) ]
      test_board1;
    test_make_word "make word DDA; board with multiple of the same letters"
      "DDAA"
      [ (0, 3); (2, 2); (3, 2); (0, 1) ]
      test_board1;
    test_make_word
      "make word DDA; board with multiple of the same letters; different order"
      "DDAA"
      [ (2, 2); (0, 3); (0, 1); (3, 2) ]
      test_board1;
    (*is_valid_word tests*)
    test_is_valid_word "is_valid_word '' test_board1" false [] test_board1;
    test_is_valid_word "is_valid_word 'A' test_board1" false
      [ (0, 1) ]
      test_board1;
    test_is_valid_word
      "is_valid_word; multiple same locations for non accpeting word \
       test_board1"
      false
      [ (0, 1); (0, 1) ]
      test_board1;
    test_is_valid_word "is_valid_word 'AT' test_board1" false
      [ (0, 1); (0, 2) ]
      test_board1;
    test_is_valid_word "is_valid_word 'CAT' test_board1" true
      [ (0, 0); (0, 1); (0, 2) ]
      test_board1;
    test_is_valid_word
      "is_valid_word 'CAT' multiple same locations for accpeting word; \
       test_board1"
      true
      [ (0, 0); (0, 1); (0, 2); (0, 0); (0, 1); (0, 2) ]
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
  "test suite for builder.ml"
  >::: List.flatten [ points_tests; trie_tests; board_tests ]

let () = run_test_tt_main suite
