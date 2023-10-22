let () = Random.self_init ()

(**
module Dictionary = struct
  (*Reads the text file [dictionary] and makes it into a list*)
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

  (** List of all the valid words in the dictionary*)
  let dictionary_list = txt_to_list "data/scrabble_dict.txt"

  (** is_word returns whether [word] is a valid word in the english dictionary*)
  let is_word word = List.mem word dictionary_list
end
*)
module BuildBoard = struct
  (*A character generator that generates letters based on how common they
    are found in the English language*)
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

  (*4x4 char array matrix*)
  let board = Array.make_matrix 4 4 'a'

  (**fill_board fills the matrix [array_2d] with random characters using 
  random_char()*)
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

  (**print_board prints the matrix [array_2d]*)
  let print_board array_2d =
    let rows = Array.length array_2d in
    let cols = Array.length array_2d.(0) in

    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        Printf.printf "%c " array_2d.(i).(j)
      done;
      print_newline ()
    done

  (** Determines if [point] is a corner tile of the board*)
  let is_corner point =
    point = (0, 0) || point = (0, 3) || point = (3, 0) || point = (3, 3)

  (** Determines if [point] is an edge tile of the board. is_edge [point] is
     false is [point] is a corner tile of the board *)
  let is_edge point =
    match is_corner point with
    | true -> false
    | false -> (
        match point with
        | x, y ->
            if x = 0 || x = 3 then y = 1 || y = 2
            else if x = 1 || x = 2 then y = 0 || y = 3
            else false)

  (** Helper function used by valid_moves. 
  Given a point (i, j), returns a list of all 8 surrounding points*)
  let possible_moves point =
    match point with
    | x, y ->
        [
          (x + 1, y);
          (x - 1, y);
          (x, y + 1);
          (x, y - 1);
          (x + 1, y + 1);
          (x + 1, y - 1);
          (x - 1, y + 1);
          (x - 1, y - 1);
        ]

  (** Helper function used by valid_moves. 
  Determines if a location [point] is within the bounds of the board*)
  let is_valid_pos point =
    match point with x, y -> x >= 0 && x <= 3 && y <= 3 && y >= 0

  (** Given a point, returns a list contain only valid locations*)
  let valid_moves point = List.filter is_valid_pos (possible_moves point)

  (** Given a character [x], finds the locations in [board] that has that 
      character. char_loc x board is a list of tuples *)
  let find_chars x board =
    let rec find_chars_helper x board i j acc =
      if i >= Array.length board then List.rev acc
      else if j >= Array.length board.(0) then
        find_chars_helper x board (i + 1) 0 acc
      else if board.(i).(j) = x then
        find_chars_helper x board i (j + 1) ((i, j) :: acc)
      else find_chars_helper x board i (j + 1) acc
    in
    find_chars_helper x board 0 0 []

  (** Given a lists of locations [lst], returns whether one of the locations is 
      valid according to valid_move*)
  let rec all_valid_chars (lst : (int * int) list) : bool =
    match lst with
    | [] -> false
    | (i, j) :: [] -> is_valid_pos (i, j)
    | (i1, j1) :: (i2, j2) :: t ->
        is_valid_pos (i1, j1)
        && List.mem (i2, j2) (valid_moves (i1, j2))
        && all_valid_chars ((i2, j2) :: t)

  let valid_next_tile start next = List.mem next (valid_moves start)

  (** Given a word [word], is_valid_word determined whether [word] is a 
    valid word in [board]. That is word is not out of bounds, is in the dictionary, 
    and each letter is of [word] is adjacent to each other and unused*)
  let is_valid_word (word : string) (board : char array array) : bool =
    let rec is_valid_word_aux str index acc =
      if index < String.length str then
        let char_at_index = String.get str index in
        let location = find_chars char_at_index board in
        all_valid_chars location && is_valid_word_aux str (index + 1) []
      else true
    in
    is_valid_word_aux word 0 []
end
