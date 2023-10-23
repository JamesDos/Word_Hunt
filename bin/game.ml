open Builder

(* REPL from A2*)
(* read-eval-print loop *)
let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ ->
      input |> eval |> print_endline;
      repl eval

module GameDict = Builder.Dictionary
module GameBoard = Builder.BuildBoard

let board = GameBoard.game_board

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to Word Hunt!\n";
  print_endline "Please enter to start the game";
  print_string "> ";
  print_endline "Starting game...";
  (*BuildBoard.print_board board;*)
  print_string "> ";
  print_endline
    "Please enter a location list to represent a word. For example \n\
     [(0, 0);(0, 1);(0,2)] would represent the top left three letters \n\
     with (0, 0) being the letter at the top left corner, (0, 1) being the \n\
     letter directly right to it etc.";
  print_string "> ";
  let input = input_line stdin in
  let string_to_tuple_list str =
    let buf = Scanf.Scanning.from_string str in
    let rec aux acc =
      match Scanf.bscanf buf " (%d, %d)%!" (fun x y -> (x, y)) with
      | exception End_of_file -> List.rev acc
      | pair -> aux (pair :: acc)
    in
    aux []
  in
  let tuples = string_to_tuple_list input in
  print_endline (string_of_bool (GameBoard.is_valid_word2 tuples board))
