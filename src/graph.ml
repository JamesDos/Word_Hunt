module type GraphSig = sig
  type node
  type t

  val make_graph : node -> node list -> t
  val add_node : t -> node -> node list -> t
  val neighbors : t -> node -> node list
end

module Board : GraphSig = struct
  type node = int * int
  type t = (node * node list) list

  let make_graph (node : node) (neighbors : node list) = [ (node, neighbors) ]

  let add_node (graph : t) (node : node) (neighbors : node list) =
    (node, neighbors) :: graph

  let rec neighbors (graph : t) (node : node) : node list =
    match graph with
    | [] -> []
    | h :: t -> if fst h = node then snd h else neighbors t node

  let to_list (graph : t) = graph
  let rec traverse graph pos = []
end

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
end
