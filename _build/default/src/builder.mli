module Dictionary : sig
  val txt_to_list : string -> string list
  val dictionary_list : string list
  val is_word : string -> bool
end

module BuildBoard : sig
  val random_char : unit -> string
  val board : string array array
  val fill_board : string array array -> string array array
  val game_board : string array array
  val print_board : string array array -> unit
  val is_corner : int * int -> bool
  val is_edge : int * int -> bool
  val possible_moves : int * int -> (int * int) list
  val is_valid_pos : int * int -> bool
  val valid_moves : int * int -> (int * int) list
  val find_chars : 'a -> 'a array array -> (int * int) list
  val is_valid_next_tile : int * int -> int * int -> bool

  val has_connection :
    (int * int) list -> (int * int) list -> bool * (int * int) list

  val is_valid_word : string -> char array array -> bool
  val char_at : int * int -> 'a array array -> 'a
  val make_word : (int * int) list -> string array array -> string
  val is_valid_word2 : (int * int) list -> string array array -> bool
end
