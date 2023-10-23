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

let () = Random.self_init ()

module BuildBoard = struct
  (*A character generator that generates letters based on how common they
    are found in the English language*)
  let random_char () =
    let cdf =
      [
        (0.111607, "E");
        (0.196573, "A");
        (0.272382, "R");
        (0.347830, "I");
        (0.419465, "O");
        (0.488974, "T");
        (0.555518, "N");
        (0.612869, "S");
        (0.667762, "L");
        (0.713150, "C");
        (0.749458, "U");
        (0.783302, "D");
        (0.814973, "P");
        (0.845102, "M");
        (0.875136, "H");
        (0.899841, "G");
        (0.920561, "B");
        (0.938682, "F");
        (0.956461, "Y");
        (0.969360, "W");
        (0.980376, "K");
        (0.990450, "V");
        (0.993352, "X");
        (0.996074, "Z");
        (0.998039, "J");
        (1.000000, "Q");
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
  let board = Array.make_matrix 4 4 "a"

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
        Printf.printf "%s " array_2d.(i).(j)
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

  (** Given a starting location [start], returns whether [next] is a valid
    next tile determined by valid_moves*)
  let is_valid_next_tile start next = List.mem next (valid_moves start)

  (** Given two lists of locations [lst1] and [lst2], returns a tuple containing
   whether any two locations between lists are adjacent and a list containing 
   tuples the points in [lst2] that connect with points in [lst1]. 
   is_connection [(0, 0); (2, 2)] [(0, 1); (3; 2)] is 
   (true, [(0, 1); (2, 3)]. 
   is_connection [(0,0); (3,3)] [(0, 2); (3, 1)] is (false, [])*)
  let rec has_connection loc_lst1 loc_lst2 =
    let rec compare_loc elm lst =
      match lst with
      | [] -> (false, [])
      | h :: t ->
          if is_valid_next_tile elm h then (true, h :: snd (compare_loc elm t))
          else (fst (compare_loc elm t), snd (compare_loc elm t))
    in
    let rec compare_lists lst1 lst2 =
      match lst1 with
      | [] -> (false, [])
      | h :: t ->
          let tup = compare_loc h lst2 in
          if fst tup then (true, snd tup @ snd (compare_lists t lst2))
          else (fst (compare_lists t lst2), snd (compare_lists t lst2))
    in
    compare_lists loc_lst1 loc_lst2

  (** Given a word [word], is_valid_word determined whether [word] is a 
    valid word in [board]. That is word is not out of bounds, is in the dictionary, 
    and each letter is of [word] is adjacent to each other*)
  let is_valid_word (word : string) (board : char array array) : bool =
    let rec is_valid_word_aux str index acc =
      if index < String.length str - 1 then
        let curr_char = String.get str index in
        let curr_locations = find_chars curr_char board in
        let next_char = String.get str (index + 1) in
        let next_locations = find_chars next_char board in
        let tup = has_connection curr_locations next_locations in
        fst tup
        && (not (List.mem (snd tup) acc))
        && is_valid_word_aux str (index + 1) (curr_locations :: acc)
      else true
    in
    String.length word > 2 && is_valid_word_aux word 0 []

  (** Given a 2d array [board], returns the character at [loc]*)
  let char_at loc board = match loc with x, y -> board.(x).(y)

  (** Given a list of locations [loc_list] and a 2d array [board],
  returns the word that is made by taking each character in [loc_list] using
  char_at and concatenating them with each other in the order they appear in*)
  let rec make_word loc_list board =
    match loc_list with
    | [] -> ""
    | h :: t -> char_at h board ^ make_word t board

  (** Given a list of locations [loc_list] and a 2d array [board], returns
  whether the word encoded by [loc_list] is a valid word according to the rules
  of word hunt. That is, the word is a valid english word, doesn't use the 
  letters from the same location multiple times and each letter is adjacent to 
  to the letter before and after it.*)
  let is_valid_word2 loc_list board =
    let rec is_valid_word2_aux loc_list acc =
      match loc_list with
      | [] -> true
      | [ x ] -> not (List.mem x acc)
      | loc1 :: loc2 :: t ->
          is_valid_next_tile loc1 loc2
          && (not (List.mem loc1 acc))
          && is_valid_word2_aux (loc2 :: t) (loc1 :: acc)
    in
    List.length loc_list > 2
    && is_valid_word2_aux loc_list []
    && List.mem (make_word loc_list board) Dictionary.dictionary_list
end
