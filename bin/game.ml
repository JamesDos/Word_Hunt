open Builder
open Str
module GameDict = Builder.Dictionary
module GameBoard = Builder.BuildBoard

let board = GameBoard.game_board

let eval_word word =
  let string_to_tuple_list str =
    let tuple_pattern = "\\(([^,]+),([^)]+)\\)" in
    let tuples_str = Str.split (Str.regexp ",") str in

    let parse_tuple tuple_str =
      if Str.string_match (Str.regexp tuple_pattern) tuple_str 0 then
        let x = int_of_string (Str.matched_group 1 tuple_str) in
        let y = int_of_string (Str.matched_group 2 tuple_str) in
        Some (x, y)
      else None
    in

    let tuples = List.filter_map parse_tuple tuples_str in
    tuples
  in
  List.length (string_to_tuple_list word) = 0
  || GameBoard.is_valid_word2 (string_to_tuple_list word) board

(* REPL from A2*)
(* read-eval-print loop *)
let rec repl (eval : string -> bool) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "Ending Game"
  | _ ->
      let result = input |> eval in
      let result_str = if result then "true" else "false" in
      print_endline result_str;
      repl eval

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to Word Hunt!\n";
  print_string "> ";
  print_endline "Starting game...";
  BuildBoard.print_board board;
  print_string "> ";
  print_endline
    "Please enter the location of letters seperated by commas torepresent a \
     word. \n\
     For example,(0,0),(0,1),(0,2) would represent the top left three letters \n\
     with (0, 0) being the letter at the top left corner, (0, 1) being the \n\
     letter directly right to it etc.";
  repl eval_word
