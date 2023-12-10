open Data_structures

let () = Random.self_init ()

module BuildBoard = struct
  (**A BuildBoard represents a 4x4 word hunt board consisting of lettered tiles.*)

  (**type of locations of letters in the board. Loc (i, j) is the letter at the
  ith row and jth column of the board.*)
  type location = Loc of int * int

  (**[random_char ()] generates singleton uppercase alphabet strings based on how 
      common the letter is found in the English language*)
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

  (**[fill_board array_2d] fills the 2d string array [array_2d] with random 
     characters using [random_char]*)
  let fill_board array_2d =
    let rows = Array.length array_2d in
    let cols = Array.length array_2d.(0) in

    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        array_2d.(i).(j) <- random_char ()
      done
    done;
    array_2d

  (** [new_board ()] generates a new 2d string array board*)
  let new_board () =
    let b = Array.make_matrix 4 4 "a" in
    fill_board b

  (**[print_board array_2d] prints the 2d string array [array_2d]*)
  let print_board array_2d =
    let rows = Array.length array_2d in
    let cols = Array.length array_2d.(0) in

    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        Printf.printf "%s " array_2d.(i).(j)
      done;
      print_newline ()
    done

  (*
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
            else false)*)

  (**[possible_moves point] is a list of all 8 surrounding locations of 
      location [point], regardless of whether [point] is within bounds of the 
      board*)
  let possible_moves point =
    match point with
    | Loc (x, y) ->
        [
          Loc (x + 1, y);
          Loc (x - 1, y);
          Loc (x, y + 1);
          Loc (x, y - 1);
          Loc (x + 1, y + 1);
          Loc (x + 1, y - 1);
          Loc (x - 1, y + 1);
          Loc (x - 1, y - 1);
        ]

  (** [is_valid_pos point] determines whether location [point] is within the 
      bounds of the board*)
  let is_valid_pos point =
    match point with Loc (x, y) -> x >= 0 && x <= 3 && y <= 3 && y >= 0

  (** [valid_moves point] is a list thats contains all points adjacent to 
       location [point]. If [point] is not within the bounds of the board, 
       returns the empty list*)
  let valid_moves point =
    if not (is_valid_pos point) then []
    else List.filter is_valid_pos (possible_moves point)

  (*
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
    find_chars_helper x board 0 0 []*)

  (**[is_valid_next_tile start next] determines whether location [next] is a 
      valid next tile to [start] as determined by [valid_moves]*)
  let is_valid_next_tile start next =
    let rec aux lst next =
      match (lst, next) with
      | [], _ -> false
      | Loc (x1, y1) :: t, Loc (x2, y2) ->
          if x1 = x2 && y1 = y2 then true else aux t next
    in
    aux (valid_moves start) next

  (** [char_at loc board] is the character at location [loc] in [board]*)
  let char_at loc board = match loc with Loc (x, y) -> board.(x).(y)

  (**[make_word loc_list board] is the word that is made by taking each location 
      in location list [loc_list], determining its character in corresponding 
      character in [board] using [char_at] and concatenating them with each 
      other in the order they appear in
      *)
  let rec make_word loc_list board =
    match loc_list with
    | [] -> ""
    | h :: t -> char_at h board ^ make_word t board

  (**[is_valid_word loc_list board] determines whether the word [w] represented 
      by location list [loc_list], as determined by [make_word], is a valid word
      in [board]. [w] is valid if it is a valid english word, as determined
      by the Scrabble Dictionary, doesn't use letters from the same location 
      multiple times, and each letter is adjacent to the letter before and after
      it.*)
  let is_valid_word loc_list board =
    let rec is_valid_word_aux loc_list acc =
      match loc_list with
      | [] -> true (*should not happen*)
      | [ x ] -> not (List.mem x acc)
      | loc1 :: loc2 :: t ->
          is_valid_next_tile loc1 loc2
          && (not (List.mem loc1 acc))
          && is_valid_word_aux (loc2 :: t) (loc1 :: acc)
    in
    List.length loc_list > 2
    && is_valid_word_aux loc_list []
    && Trie.search_word Dictionary.trie
         (Trie.to_char_list (make_word loc_list board))

  (**[traverse grid i j word order solutions] is a helper function called by
      [solve]. Given a 2d array of strings [grid], an [i] and [j] position on 
      the board [grid] and accumulator string [word], a location list [order],
      and a hashtable [solutions] maping strings to their order,  
      [traverse grid i j word order solutions], finds all possible words that 
      can be made from the [grid] starting at location (i, j), maps these words 
      to the order they were made in and inserts these bindings into
      [solutions].*)
  let rec traverse (grid : string array array) (i : int) (j : int)
      (word : string) (order : location list)
      (solutions : (string, location list) Hashtbl.t) : unit =
    (* creates a copied grid *)
    let grid' = Array.map (fun a -> Array.copy a) grid in
    let char = grid'.(i).(j) in
    let word = word ^ char in
    let new_order = order @ [ Loc (i, j) ] in
    (*If word is not in trie, then prunes by ending traversal*)
    if not (Trie.search_word Dictionary.trie (Trie.to_char_list word)) then ()
    else Hashtbl.add solutions word new_order;

    (* Mark the current cell as visited by setting to an empty string *)
    grid'.(i).(j) <- "";
    let neighbors = valid_moves (Loc (i, j)) in
    List.iter
      (fun loc ->
        match loc with
        | Loc (next_i, next_j) -> (
            if
              next_i >= 0
              && next_i < Array.length grid
              && next_j >= 0
              && next_j < Array.length grid.(0)
            then
              (*Backtracking*)
              match grid'.(next_i).(next_j) with
              | cell when cell <> "" ->
                  traverse grid' next_i next_j word new_order solutions
              | _ -> ()))
      neighbors;
    ()

  (** Given a 2d string array representing a board [grid] and a hashtable 
      [solutions] maping strings to locations lists representing orders, 
      [solve grid solutions] inserts into [solutions] all possible valid 
      english words of length > 2 that can be generated from [grid], mapped to 
      the order (represented by a location list) that they were created in.
      Requires that [hashtable] is an empty hashtable.*)

  let solve grid solutions =
    for i = 0 to Array.length grid - 1 do
      for j = 0 to Array.length grid.(0) - 1 do
        traverse grid i j "" [] solutions
      done
    done

  (**[solutions hashtable] is a list of all keys in [hashtable], a hastable that
      represents the solutions of the [board] that [hashtable] was called with
      when called with [solve]. The list generated is a list of string containing
      all the solutions in the grid, ordered from longest to shortest length.
      Requires: [solve] to be called on [hashtable] before calling [solutions] 
      on [hashtable]
      *)
  let solutions hashtable =
    let getKeys (tbl : (string, location list) Hashtbl.t) : 'a list =
      let key_list = ref [] in
      Hashtbl.iter (fun key _ -> key_list := key :: !key_list) tbl;
      !key_list
    in
    let all_words = getKeys hashtable in
    List.filter (fun x -> String.length x > 2) all_words

  (**[longest_words word_list n] is a list containg the [n] longest words in 
      [word_list]. This list is sorted decreasingly from longest length words. 
      If [n] is greater than the length of [word_list], then the returned list
      is the same length as [word_list]*)
  let longest_words word_lst n =
    let sorted =
      let compare = String.compare in
      let no_dups = List.sort_uniq compare word_lst in
      List.sort
        (fun x y ->
          if String.length x < String.length y then 1
          else if String.length y < String.length x then -1
          else 0)
        no_dups
    in
    let rec take_n word_lst n acc =
      match word_lst with
      | [] -> List.rev acc
      | h :: t -> if n = 0 then List.rev acc else take_n t (n - 1) (h :: acc)
    in
    take_n sorted n []
end
