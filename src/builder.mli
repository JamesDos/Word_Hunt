module BuildBoard : sig
  (**A BuildBoard represents a 4x4 word hunt board consisting of lettered tiles.*)

  (**type of locations of letters in the board. Loc (i, j) is the letter at the
  ith row and jth column of the board.*)
  type location = Loc of int * int

  val random_char : unit -> string
  (**[random_char ()] generates singleton uppercase alphabet strings based on how 
      common the letter is found in the English language*)

  val fill_board : string array array -> string array array
  (**[fill_board array_2d] fills the 2d string array [array_2d] with random 
     characters using [random_char]*)

  val new_board : unit -> string array array
  (** [new_board ()] generates a new 2d string array board*)

  val print_board : string array array -> unit
  (**[print_board array_2d] prints the 2d string array [array_2d]*)

  val possible_moves : location -> location list
  (**[possible_moves point] is a list of all 8 surrounding locations of 
      location [point], regardless of whether [point] is within bounds of the 
      board*)

  val is_valid_pos : location -> bool
  (** [is_valid_pos point] determines whether location [point] is within the 
      bounds of the board*)

  val valid_moves : location -> location list
  (** [valid_moves point] is a list thats contains all points adjacent to 
       location [point]. If [point] is not within the bounds of the board, 
       returns the empty list*)

  val is_valid_next_tile : location -> location -> bool

  (**[is_valid_next_tile start next] determines whether location [next] is a 
      valid next tile to [start] as determined by [valid_moves]*)

  val char_at : location -> 'a array array -> 'a
  (** [char_at loc board] is the character at location [loc] in [board]*)

  val make_word : location list -> string array array -> string
  (**[make_word loc_list board] is the word that is made by taking each location 
      in location list [loc_list], determining its character in corresponding 
      character in [board] using [char_at] and concatenating them with each 
      other in the order they appear in
      *)

  val is_valid_word : location list -> string array array -> bool
  (**[is_valid_word loc_list board] determines whether the word [w] represented 
      by location list [loc_list], as determined by [make_word], is a valid word
      in [board]. [w] is valid if it is a valid english word, as determined
      by the Scrabble Dictionary, doesn't use letters from the same location 
      multiple times, and each letter is adjacent to the letter before and after
      it.*)

  val traverse :
    string array array ->
    int ->
    int ->
    string ->
    location list ->
    (string, location list) Hashtbl.t ->
    unit

  (**[traverse grid i j word order solutions] is a helper function called by
      [solve]. Given a 2d array of strings [grid], an [i] and [j] position on 
      the board [grid] and accumulator string [word], a location list [order],
      and a hashtable [solutions] maping strings to their order,  
      [traverse grid i j word order solutions], finds all possible words that 
      can be made from the [grid] starting at location (i, j), maps these words 
      to the order they were made in and inserts these bindings into
      [solutions].*)

  val solve : string array array -> (string, location list) Hashtbl.t -> unit
  (** Given a 2d string array representing a board [grid] and a hashtable 
      [solutions] maping strings to locations lists representing orders, 
      [solve grid solutions] inserts into [solutions] all possible valid 
      english words of length > 2 that can be generated from [grid], mapped to 
      the order (represented by a location list) that they were created in.
      Requires that [hashtable] is an empty hashtable.*)

  val solutions : (string, location list) Hashtbl.t -> string list
  (**[solutions hashtable] is a list of all keys in [hashtable], a hastable that
      represents the solutions of the [board] that [hashtable] was called with
      when called with [solve]. The list generated is a list of string containing
      all the solutions in the grid, ordered from longest to shortest length.
      Requires: [solve] to be called on [hashtable] before calling [solutions] 
      on [hashtable]
      *)

  val longest_words : string list -> int -> string list
  (**[longest_words word_list n] is a list containg the [n] longest words in 
      [word_list]. This list is sorted decreasingly from longest length words. 
      If [n] is greater than the length of [word_list], then the returned list
      is the same length as [word_list]*)
end
