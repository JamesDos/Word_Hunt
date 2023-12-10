(**Data_structures contains the modules [Trie] and [Dictionary]*)

module Trie : sig
  (** A Trie is a tree of characters used to efficiently store and lookup words 
      in the english language*)

  type trie_node = {
    value : char option;
    mutable is_end_of_word : bool;
    children : (char, trie_node) Hashtbl.t;
  }
  (**trie_node represents a node in this trie*)

  val create_node : unit -> trie_node
  (**[create_node ()] creates an empty trie*)

  val insert_word : char list -> trie_node -> unit
  (**[insert_word node word] inserts char list [word] into the trie [node]*)

  val search_word : trie_node -> char list -> bool
  (**[search_word node word] returns whether [word] is a present in the trie [node]*)

  val to_char_list : string -> char list
  (** [to_char_list word] is a list of characters of [word] in their original order.
      Example: [to_char_list "cat"] is ['c'; 'a'; 't'] *)

  val insert_list_of_words : trie_node -> string list -> unit
  (**[insert_list_of_words node words] inserts each word in string list [words] 
      into trie [node]*)

  val to_string : trie_node -> string
  (**[to_string trie] returns a string representation of [trie]*)
end

module Dictionary : sig
  (**A Dictionary represents the scrabble dictionary used to verify valid english
    words in word hunt*)

  val txt_to_list : string -> string list
  (**[txt_to_list dictionary] reads the string name of the text file [dictionary] 
      and makes it into a list with each element of the list being a word in the
      text file [dictionary]. Requires that each line in [dictionary].txt contains
      only one string*)

  val dictionary_list : string list
  (** [dictionary_list] is a list of all the valid words according to the
       scrabble dictionary*)

  val is_word : string -> bool
  (** [is_word word ] returns whether [word] is a valid word in the scrabble 
      dictionary*)

  val trie : Trie.trie_node
  (**[trid] is a trie representation of the scrabble dictionary*)
end
