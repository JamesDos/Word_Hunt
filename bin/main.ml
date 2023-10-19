(*Testing*)

(*A character generator that generates letters based on how common they
  are found in the English language*)

let () = Random.self_init ()

let random_char () =
  let cdf =
    [
      (0.111607, 'E');
      (0.196573, 'A');
      (0.272382, 'R');
      (0.347830, 'I');
      (0.419465, 'O');
      (0.488974, 'T');
      (0.555518, 'N');
      (0.612869, 'S');
      (0.667762, 'L');
      (0.713150, 'C');
      (0.749458, 'U');
      (0.783302, 'D');
      (0.814973, 'P');
      (0.845102, 'M');
      (0.875136, 'H');
      (0.899841, 'G');
      (0.920561, 'B');
      (0.938682, 'F');
      (0.956461, 'Y');
      (0.969360, 'W');
      (0.980376, 'K');
      (0.990450, 'V');
      (0.993352, 'X');
      (0.996074, 'Z');
      (0.998039, 'J');
      (1.000000, 'Q');
    ]
  in
  let rand = Random.float 1.0 in
  let rec find_char letter =
    match letter with
    | [] -> failwith "Should not happen"
    | (p, c) :: t -> if rand <= p then c else find_char t
  in
  find_char cdf

(*Reads the text file Dictionary and makes it into a list*)
let txt_to_list dictionary =
  let ic = open_in dictionary in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  loop []

let dictionary_list = txt_to_list "Dictionary"
let is_word word = List.mem word dictionary_list
let board = Array.make_matrix 4 4 'a'

let fill_board array_2d =
  let rows = Array.length array_2d in
  let cols = Array.length array_2d.(0) in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      array_2d.(i).(j) <- random_char ()
    done
  done;
  array_2d

let game_board = fill_board board

let print_board array_2d =
  let rows = Array.length array_2d in
  let cols = Array.length array_2d.(0) in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      Printf.printf "%c " array_2d.(i).(j)
    done;
    print_newline ()
  done

let () = print_board game_board
let () = print_string (string_of_bool (is_word "zniocius"))
