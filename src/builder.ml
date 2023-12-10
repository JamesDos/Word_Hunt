open Data_structures

let () = Random.self_init ()

module BuildBoard = struct
  type location = Loc of int * int

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

  let fill_board array_2d =
    let rows = Array.length array_2d in
    let cols = Array.length array_2d.(0) in

    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        array_2d.(i).(j) <- random_char ()
      done
    done;
    array_2d

  let new_board () =
    let b = Array.make_matrix 4 4 "a" in
    fill_board b

  let print_board array_2d =
    let rows = Array.length array_2d in
    let cols = Array.length array_2d.(0) in

    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        Printf.printf "%s " array_2d.(i).(j)
      done;
      print_newline ()
    done

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

  let is_valid_pos point =
    match point with Loc (x, y) -> x >= 0 && x <= 3 && y <= 3 && y >= 0

  let valid_moves point =
    if not (is_valid_pos point) then []
    else List.filter is_valid_pos (possible_moves point)

  let is_valid_next_tile start next =
    let rec aux lst next =
      match (lst, next) with
      | [], _ -> false
      | Loc (x1, y1) :: t, Loc (x2, y2) ->
          if x1 = x2 && y1 = y2 then true else aux t next
    in
    aux (valid_moves start) next

  let char_at loc board = match loc with Loc (x, y) -> board.(x).(y)

  let rec make_word loc_list board =
    match loc_list with
    | [] -> ""
    | h :: t -> char_at h board ^ make_word t board

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

  let solve grid solutions =
    for i = 0 to Array.length grid - 1 do
      for j = 0 to Array.length grid.(0) - 1 do
        traverse grid i j "" [] solutions
      done
    done

  let solutions hashtable =
    let getKeys (tbl : (string, location list) Hashtbl.t) : 'a list =
      let key_list = ref [] in
      Hashtbl.iter (fun key _ -> key_list := key :: !key_list) tbl;
      !key_list
    in
    let all_words = getKeys hashtable in
    List.filter (fun x -> String.length x > 2) all_words

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
