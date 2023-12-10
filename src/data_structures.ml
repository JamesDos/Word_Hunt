module Trie = struct
  type trie_node = {
    value : char option;
    mutable is_end_of_word : bool;
    children : (char, trie_node) Hashtbl.t;
  }

  let create_node () =
    { value = None; is_end_of_word = false; children = Hashtbl.create 10 }

  let rec insert_word word node =
    match word with
    | [] -> node.is_end_of_word <- true
    | hd :: tl ->
        let next_node =
          try Hashtbl.find node.children hd
          with Not_found ->
            let new_node = create_node () in
            Hashtbl.add node.children hd new_node;
            new_node
        in
        insert_word tl next_node

  let rec search_word node word =
    match word with
    | [] -> node.is_end_of_word
    | hd :: tl -> (
        try
          let next_node = Hashtbl.find node.children hd in
          search_word next_node tl
        with Not_found -> false)

  let to_char_list word = List.of_seq (String.to_seq word)

  let insert_list_of_words node words =
    List.iter (fun word -> insert_word (to_char_list word) node) words

  let to_string trie =
    let rec to_string_helper node depth =
      let indent = String.make (depth * 2) ' ' in
      let value_str =
        match node.value with
        | Some c -> String.make 1 c
        | None -> if depth = 0 then "ROOT" else ""
      in
      let end_of_word_str = if node.is_end_of_word then " (EOW)" else "" in
      let children_str =
        Hashtbl.fold
          (fun char child acc ->
            acc ^ to_string_helper child (depth + 1) ^ "\n")
          node.children ""
      in
      indent ^ value_str ^ end_of_word_str ^ "\n" ^ children_str
    in
    to_string_helper trie 0
end

module Dictionary = struct
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

  let dictionary_list = txt_to_list "data/scrabble_dict.txt"
  let is_word word = List.mem word dictionary_list

  let trie =
    let root = Trie.create_node () in
    Trie.insert_list_of_words root dictionary_list;
    root
end
