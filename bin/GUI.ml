open Bogue
open Tsdl
open Unix
open Thread
module W = Widget
module L = Layout
module T = Trigger
module GameBoard = Builder.BuildBoard

(*[board] is a 2d string array generated by GameBoard*)
let board = GameBoard.game_board

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_tuple (s : GameBoard.location) =
  match s with
  | Loc (x, y) -> "\"" ^ string_of_int x ^ " " ^ string_of_int y ^ "\""

let main () =
  let width = 400 in
  (*[score] keeps track of the players current score *)
  let score = ref 0 in
  let word_field = W.label ~size:40 "" in

  (*entered_word_locs an array tracking the locations of letters that the user has
    inputted in the order that they were inputed*)
  let entered_locs = ref [] in

  let array_of_list lst = Array.of_list (List.rev !lst) in

  (*[add_loc loc] mutates [entered_words_locs] by appending location [loc] to it*)
  let add_loc (loc : GameBoard.location) =
    entered_locs := loc :: !entered_locs
  in

  (*[remove_loc _] mutates [entered_words_locs] by removing its last location.
     Requires that [entered_words_locs] is nonempty*)
  let remove_loc _ =
    let new_locs =
      if List.length !entered_locs < 1 then !entered_locs
      else List.tl !entered_locs
    in
    entered_locs := new_locs
  in

  (*[board_matrix] is a 2d array storing each tiles of [board]*)
  let board_matrix = Array.make_matrix 4 4 (W.button "") in

  (*[get_button loc] gets the button at location [loc] in [board_matrix]*)
  let get_button loc =
    match loc with GameBoard.Loc (i, j) -> board_matrix.(i).(j)
  in

  (* [add_letter l] appends [l] to [word_field]'s text and sets this string as
      its new text*)
  let add_letter l = W.set_text word_field (W.get_text word_field ^ l) in

  (* [remove_letter _] removes the last letter of [word_field]'s text and sets
     this string as its new text.
      Requires that the text of [word_field] is nonempty*)
  let remove_letter _ =
    let input_word_text = W.get_text word_field in
    let length = String.length input_word_text in
    let new_text =
      if length < 1 then "" else String.sub input_word_text 0 (length - 1)
    in
    W.set_text word_field new_text
  in

  (*[entered_words] is an array of the valid words entered by the player*)
  let entered_words = ref [] in

  (*[add_word word] mutates [entered_words] by appending [word] to it. Requires
    [word] is not a member of [entered_words]*)
  let add_word word = entered_words := word :: !entered_words in

  let score_board = W.label ~size:40 (string_of_int !score) in

  (*[score_word word] gives [word] an int score based on its length, according to the
     scoring rules of word hunt. Requires that the length of [word] > 2*)
  let score_word word = 400 * (String.length word - 2) in

  (*[update_score word] mutates [score] and updates the score based off of the score
     of [word] determiend by score_word*)
  let update_score word =
    if String.length word > 2 then score := !score + score_word word;
    W.set_text score_board (string_of_int !score)
  in

  (*[is_valid_tile loc] returns whether the tile at location [loc] is valid.
     That is if the tile at [loc] is adjacent to the last tile of [entered_locs]
      and the tile at [loc] is not already a tile in [entered_locs]*)
  let is_valid_tile loc =
    let entered_locs_arr = array_of_list entered_locs in
    let length = Array.length entered_locs_arr in
    if length < 1 then true
    else
      let last = List.hd !entered_locs in
      (GameBoard.is_valid_next_tile last loc
      ||
      match (loc, List.hd !entered_locs) with
      | Loc (x1, y1), Loc (x2, y2) -> x1 = x2 && y1 = y2)
      && not (List.mem loc (List.tl !entered_locs))
  in

  (* [make_tile letter] creates a button representing a word tile with [letter]
     as its text. If button is  switched from off to on [add_letter letter] and
     [add_loc] are called. If button is switched from on to off [remove_letter]
     and [remove_loc] are called *)
  let make_tile letter i j =
    let tile_label = Label.create ~size:50 letter in
    let loc = GameBoard.Loc (i, j) in
    let action b =
      let valid_click = is_valid_tile loc in
      if not valid_click then (
        let button = board_matrix.(i).(j) in
        print_endline
          (string_of_bool
             (match (loc, List.hd !entered_locs) with
             | Loc (x1, y1), Loc (x2, y2) -> x1 = x2 && y1 = y2));
        if List.mem loc !entered_locs then W.set_state button true
        else W.set_state button false)
      else if b then (
        add_letter letter;
        add_loc (GameBoard.Loc (i, j)))
      else (
        remove_letter letter;
        remove_loc letter)
    in
    W.button
      ~bg_off:(Draw.(opaque yellow) |> Style.color_bg)
      ~bg_on:(Draw.(opaque red) |> Style.color_bg)
      ~border_radius:8 ~label:tile_label ~action ~kind:Button.Switch letter
  in

  (*[make_rowi acc i j] generates a list of Layout residents that are buttons
    representing the i, j th position of [board]. Each button at location (i, j)
    is also appended to [board_matrix]*)
  let rec make_rowi acc i j =
    if j < 0 then acc
    else
      let button = make_tile board.(i).(j) i j in
      board_matrix.(i).(j) <- button;
      make_rowi (L.resident ~w:(width / 4) ~h:70 button :: acc) i (j - 1)
  in

  (*creating the board*)
  let board_row1 = make_rowi [] 0 3 |> L.flat in
  let board_row2 = make_rowi [] 1 3 |> L.flat in
  let board_row3 = make_rowi [] 2 3 |> L.flat in
  let board_row4 = make_rowi [] 3 3 |> L.flat in
  let board_array = [ board_row1; board_row2; board_row3; board_row4 ] in

  let game_board =
    L.tower ~hmargin:(width / 2) ~align:Draw.Center board_array
  in

  let input_word_field =
    L.flat [ L.resident ~w:width word_field; L.resident ~w:width score_board ]
  in

  let reset_tiles matrix =
    for i = 0 to 3 do
      for j = 0 to 3 do
        Button.reset (W.get_button matrix.(i).(j));
        entered_locs := []
      done
    done
  in
  let reset_text_field m =
    let field_word = W.get_text word_field in
    if
      (not (GameBoard.is_valid_word (List.rev !entered_locs) board))
      || List.mem field_word !entered_words
    then (
      print_endline "not a valid word";
      print_endline (GameBoard.make_word (List.rev !entered_locs) board))
    else (
      print_endline "is a valid word";
      add_word field_word;
      update_score field_word;
      W.set_text word_field "";
      reset_tiles m)
  in

  let enter_button =
    let action _ = reset_text_field board_matrix in
    W.button ~action ~kind:Button.Trigger "Enter Word"
  in

  let enter_button_flat =
    L.flat ~margins:100 [ L.resident ~w:(width * 2) enter_button ]
  in

  (*
  let input_word = W.label ~size:40 "" in
  let update_word_action = W.map_text (fun s -> s) in
  let c_input = W.connect input_word button update_word_action in
  Sdl.Event[text_input; mouse_button_down] *)

  (*
  let a = board in
  let headers = [ "Column 1"; "Column 2"; "Column 3"; "Column 4" ] in
  let widths = [ Some 100; Some 100; Some 100; Some 100 ] in*)

  (*let table, _ = Table.of_array ~h:400 ~widths headers a in*)
  let action input label _ =
    let text = W.get_text input in
    W.set_text label ("Hello " ^ text ^ "!")
  in

  let input =
    W.button
      ~bg_off:(Draw.(opaque yellow) |> Style.color_bg)
      ~bg_on:(Draw.(opaque red) |> Style.color_bg)
      ~kind:Button.Switch "A"
  in

  (*let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in*)
  let label = W.label ~size:40 "Hello!" in

  let c = W.connect input label action Sdl.Event.[ mouse_button_down ] in

  let layout =
    L.tower [ L.resident ~w:400 input; L.resident ~w:400 ~h:200 label ]
  in

  (*TODO: Make lists for all user inputted words and longest words that could have been made*)
  let convert_str_lst_to_str str_lst =
    let formatted_lines =
      List.mapi (fun i s -> Printf.sprintf "%d. %s" (i + 1) s) str_lst
    in
    String.concat "\n" formatted_lines
  in

  let board_solutions =
    let hashtable = Hashtbl.create 10 in
    GameBoard.solve board hashtable;
    GameBoard.solutions hashtable
  in

  let top_user_words = !entered_words in

  let top_possible_words = GameBoard.longest_words board_solutions 20 in

  let top_user_words_display =
    W.text_display ~w:100 ~h:630 (convert_str_lst_to_str top_user_words)
  in

  let top_possible_words_display =
    W.text_display ~w:100 ~h:630 (convert_str_lst_to_str top_possible_words)
  in

  let top_possible_words_title =
    W.rich_text ~size:20 ~w:width ~h:30
      Text_display.(page [ bold (para "Longest Possibe Words") ])
  in

  let top_user_words_title =
    W.rich_text ~size:20 ~w:width ~h:30
      Text_display.(page [ bold (para "Your Words") ])
  in

  let top_words_list =
    L.tower
      [
        L.resident top_possible_words_title;
        L.resident top_possible_words_display;
      ]
  in

  let user_words_list =
    L.tower
      [ L.resident top_user_words_title; L.resident top_user_words_display ]
  in
  let display_lists = L.flat [ top_words_list; user_words_list ] in

  let ending_message = W.label ~size:50 " Thanks for playing!" in

  let score_message =
    W.label ~size:30 ("Final score: " ^ string_of_int !score)
  in

  let page1 =
    L.tower ~background:(Draw.(opaque (88, 245, 77)) |> L.color_bg) [ layout ]
  in

  let page2 =
    L.tower
      ~background:(Draw.(opaque (88, 245, 77)) |> L.color_bg)
      [ input_word_field; game_board; enter_button_flat ]
  in

  let page3 =
    L.tower
      ~background:(Draw.(opaque (88, 245, 77)) |> L.color_bg)
      [
        L.resident ~w:1000 ~h:60 ending_message;
        display_lists;
        L.resident ~w:1000 ~h:50 score_message;
      ]
  in

  let timer_label = W.label ~size:40 "60" in
  let update_timer () =
    let rec loop remaining_time =
      if remaining_time >= 0 then (
        W.set_text timer_label (string_of_int remaining_time);
        Unix.sleep 1;
        loop (remaining_time - 1))
    in
    ignore (Thread.create loop 60)
  in

  (*let page2 = L.tower [ layout ] in*)
  let tabs =
    Tabs.create ~slide:Avar.Right
      [ ("Page 1", page1); ("Page 2", page2); ("Page 3", page3) ]
  in
  (*let layout = L.tower [ L.resident (W.label "Word Hunt"); table ] in*)
  let board = Bogue.of_layout ~connections:[ c ] tabs in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()

(* let timer =
   Thread.create (fun () ->
       Unix.sleep 60;
       Tabs.goto_page tabs 2) *)
