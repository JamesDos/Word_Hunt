module Trie : sig
  type trie_node = {
    value : char option;
    mutable is_end_of_word : bool;
    children : (char, trie_node) Hashtbl.t;
  }

  val create_node : unit -> trie_node
  val insert_word : char list -> trie_node -> unit
  val search_word : trie_node -> char list -> bool
  val to_char_list : string -> char list
  val insert_list_of_words : trie_node -> string list -> unit
  val to_string : trie_node -> string
end

module Dictionary : sig
  val txt_to_list : string -> string list
  val dictionary_list : string list
  val is_word : string -> bool
  val trie : Trie.trie_node
end
