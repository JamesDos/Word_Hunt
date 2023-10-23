open Builder
open Str
module GameDict = Builder.Dictionary
module GameBoard = Builder.BuildBoard

let board = GameBoard.game_board

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
     For example,(0,0);(0,1);(0,2) would represent the top left three letters \n\
     with (0,0) being the letter at the top left corner, (0,1) being the \n\
     letter directly right to it etc.";

  (* Read user input and parse it into a list of tuples *)
  let input = read_line () in
  let locations =
    input |> String.split_on_char ';'
    |> List.map (fun s -> Scanf.sscanf s "(%d,%d)" (fun x y -> (x, y)))
  in

  (* Print the list of tuples *)
  List.iter (fun (x, y) -> Printf.printf "(%d,%d)\n" x y) locations;

  (* Check if the word is valid and print the result *)
  if BuildBoard.is_valid_word2 locations board then
    print_endline "The word is valid!"
  else print_endline "The word is not valid."
