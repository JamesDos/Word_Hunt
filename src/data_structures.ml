module Trie = struct
  type trie_node = {
    value : char option;
    mutable is_end_of_word : bool;
    children : (char, trie_node) Hashtbl.t;
  }

  let create_node () =
    { value = None; is_end_of_word = false; children = Hashtbl.create 10 }

  let rec insert_word node word =
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
        insert_word next_node tl

  let rec search_word node word =
    match word with
    | [] -> node.is_end_of_word
    | hd :: tl -> (
        try
          let next_node = Hashtbl.find node.children hd in
          search_word next_node tl
        with Not_found -> false)

  let insert_list_of_words node words =
    List.iter
      (fun word -> insert_word node (List.of_seq (String.to_seq word)))
      words

  let to_char_list word = List.of_seq (String.to_seq word)
end

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

  let trie =
    let root = Trie.create_node () in
    Trie.insert_list_of_words root dictionary_list;
    root
end
