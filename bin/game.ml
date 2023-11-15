open Builder
open Data_structures
open Str
module GameDict = Data_structures.Dictionary
module GameBoard = Builder.BuildBoard

let board = GameBoard.game_board

let eval_input str =
  let string_to_tuple_list str =
    str |> String.split_on_char ';'
    |> List.map (fun s -> Scanf.sscanf s "(%d,%d)" (fun x y -> (x, y)))
  in
  BuildBoard.is_valid_word2 (string_to_tuple_list str) board

(* read-eval-print loop *)
let rec repl (eval : string -> bool) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "Ending Game"
  | _ ->
      (let res = input |> eval in
       if res then "Valid Word" else "Invalid Word")
      |> print_endline;
      repl eval

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to Word Hunt!\n";
  print_string "> ";
  print_endline "Starting game...";
  BuildBoard.print_board board;
  print_string "> ";
  print_endline
    "Please enter the location of letters seperated by commas to represent a \
     word. \n\
     For example,(0,0);(0,1);(0,2) would represent the top left three letters \n\
     with (0,0) being the letter at the top left corner, (0,1) being the \n\
     letter directly right to it etc. Requires that the format of the input is\n\
     is exactly like the example. That is, there can't be any spaces between or\n\
     within the tuples and no non-tuple strings. So (0, 0), (0,1) ; (0,2), and \
     'hello' would\n\
     be invalid inputs and crash the game.";
  repl eval_input
