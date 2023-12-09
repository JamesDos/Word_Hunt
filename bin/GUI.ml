open Bogue
open Tsdl
open Unix
open Thread
module W = Widget
module L = Layout
module T = Trigger
module GameBoard = Builder.BuildBoard

(*[board] is a 2d string array generated by GameBoard*)
let board = ref (GameBoard.new_board ())

let reset_board () =
  let module New_board = Builder.BuildBoard in
  New_board.board

(*used for debugging*)
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
  (*useful functions*)
  let array_of_list lst = Array.of_list (List.rev !lst) in
  let convert_str_lst_to_str str_lst =
    let formatted_lines =
      List.mapi (fun i s -> Printf.sprintf "%d. %s" (i + 1) s) str_lst
    in
    String.concat "\n" formatted_lines
  in

  let width = 400 in

  (*variables to keep track of score, entered locations, entered words and
    their functions*)
  let score = ref 0 in

  let entered_locs = ref [] in

  (*first element of has_entered_word is if the user has entered a valid word,
    second element of has entered_word is if the state of the first element has
    been toggled previously by the enter button or timer. If the state has been
    toggled by the enter button then snd !has_entered_word is false, otherwise
    its true. Third element is how much extra time user should get if they input
    a valid word)
  *)
  let has_entered_word = ref (false, false, 0) in

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

  (*[entered_words] is an array of the valid words entered by the player*)
  let entered_words = ref [] in

  (*[add_word word] mutates [entered_words] by appending [word] to it. Requires
    [word] is not a member of [entered_words]*)
  let add_word word = entered_words := word :: !entered_words in

  let toggle_has_entered_word_enter n = has_entered_word := (true, false, n) in

  let toggle_has_entered_word_timer () = has_entered_word := (false, true, 0) in

  let calculate_extra_time word =
    match String.length word with
    | 0 | 1 | 2 -> 0
    | 3 -> 5
    | 4 -> 7
    | 5 -> 12
    | 6 -> 20
    | _ -> 30
  in

  (*Page 1 ********************************************************************)
  let start_normal_button =
    W.button
      ~bg_off:(Draw.(opaque yellow) |> Style.color_bg)
      ~bg_on:(Draw.(opaque red) |> Style.color_bg)
      ~kind:Button.Switch "Play"
  in

  let start_survival_button =
    W.button
      ~bg_off:(Draw.(opaque yellow) |> Style.color_bg)
      ~bg_on:(Draw.(opaque red) |> Style.color_bg)
      ~kind:Button.Switch "Play Survival Mode"
  in

  (*let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in*)
  let welcome_label = W.label ~size:20 "Welcome to Word Hunt!" in

  let page1 =
    L.tower ~hmargin:500
      [
        L.resident ~w:500 ~h:200 welcome_label;
        L.resident ~w:500 ~h:50 start_normal_button;
        L.resident ~w:500 ~h:50 start_survival_button;
      ]
  in

  (*Page 2 ********************************************************************)
  let word_field = W.label ~size:40 "" in

  (*[board_matrix] is a 2d array storing each tiles of [board]*)
  let board_matrix = Array.make_matrix 4 4 (W.button "", W.label "") in

  (*
  (*[get_button loc] gets the button at location [loc] in [board_matrix]*)
  let get_button loc =
    match loc with GameBoard.Loc (i, j) -> board_matrix.(i).(j)
  in *)

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

  let score_board = W.label ~size:40 ("Score: " ^ string_of_int !score) in

  (*[score_word word] gives [word] an int score based on its length, according to the
     scoring rules of word hunt. Requires that the length of [word] > 2*)
  let score_word word =
    let score = String.length word in
    match score with
    | 0 | 1 | 2 -> 0
    | 3 -> 100
    | 4 -> 400
    | 5 -> 800
    | 6 -> 1400
    | 7 -> 1800
    | _ -> 2500
  in

  let score_message =
    W.label ~size:30 ("Final score: " ^ string_of_int !score)
  in

  (*[update_score word] mutates [score] and updates the score based off of the score
     of [word] determiend by score_word*)
  let update_score word =
    if String.length word > 2 then score := !score + score_word word;
    W.set_text score_board ("Score: " ^ string_of_int !score);
    W.set_text score_message ("Final score: " ^ string_of_int !score)
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
  let make_tile i j =
    let loc = GameBoard.Loc (i, j) in
    let action b =
      let letter = !board.(i).(j) in
      let valid_click = is_valid_tile loc in
      if not valid_click then (
        let button, _ = board_matrix.(i).(j) in
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
    let letter = !board.(i).(j) in
    let tile_label_widget = W.label ~size:50 letter in
    let tile_label = W.get_label tile_label_widget in

    let tile_button =
      W.button
        ~bg_off:(Draw.(opaque yellow) |> Style.color_bg)
        ~bg_on:(Draw.(opaque red) |> Style.color_bg)
        ~border_radius:8 ~label:tile_label ~action ~kind:Button.Switch letter
    in
    (tile_button, tile_label_widget)
  in

  (*[make_rowi acc i j] generates a list of Layout residents that are buttons
    representing the i, j th position of [board]. Each button at location (i, j)
    is also appended to [board_matrix]*)
  let rec make_rowi acc i j =
    if j < 0 then acc
    else
      let button, label = make_tile i j in
      board_matrix.(i).(j) <- (button, label);
      make_rowi (L.resident ~w:(width / 4) ~h:70 button :: acc) i (j - 1)
  in

  (*creating the board*)
  let board_row1 = make_rowi [] 0 3 |> L.flat in
  let board_row2 = make_rowi [] 1 3 |> L.flat in
  let board_row3 = make_rowi [] 2 3 |> L.flat in
  let board_row4 = make_rowi [] 3 3 |> L.flat in
  let board_array = [ board_row1; board_row2; board_row3; board_row4 ] in

  let game_board =
    L.tower ~hmargin:(width / 3) ~align:Draw.Center board_array
  in

  let used_words_field = W.text_display "" in

  let used_words_display =
    W.rich_text ~size:20 ~w:width ~h:30
      Text_display.(page [ bold (para "Your Words") ])
  in

  let used_words_layout =
    let contents = L.resident ~h:1000 used_words_field in
    L.make_clip ~h:300 contents
  in

  let used_words_tower =
    L.tower [ L.resident used_words_display; used_words_layout ]
  in

  let top_user_words = !entered_words in

  let top_user_words_display =
    W.text_display ~w:100 ~h:630 (convert_str_lst_to_str top_user_words)
  in

  let update_used_words_field lst =
    let text = String.concat "\n" lst in
    W.set_text used_words_field text;
    W.set_text top_user_words_display text
  in

  let reset_tiles matrix =
    for i = 0 to 3 do
      for j = 0 to 3 do
        let button, _ = matrix.(i).(j) in
        Button.reset (W.get_button button);
        entered_locs := []
      done
    done
  in

  let reset_text_field m =
    let field_word = W.get_text word_field in
    if
      true (* set to false to always accept every word *)
      && ((not (GameBoard.is_valid_word (List.rev !entered_locs) !board))
         || List.mem field_word !entered_words)
    then (
      print_endline "not a valid word";
      print_endline (GameBoard.make_word (List.rev !entered_locs) !board))
    else (
      print_endline "is a valid word";
      add_word field_word;
      update_score field_word;
      update_used_words_field (List.rev !entered_words));
    toggle_has_entered_word_enter (calculate_extra_time field_word);
    W.set_text word_field "";
    reset_tiles m
  in

  let enter_button =
    let action _ = reset_text_field board_matrix in
    W.button ~action ~kind:Button.Trigger "Enter Word"
  in

  let enter_button_flat =
    L.flat ~margins:100 [ L.resident ~w:(width * 2) enter_button ]
  in

  (* [assign_label i j] assigns the label letter to the tile button at position
     i j based on the board *)
  let assign_label_letter i j brd =
    let letter = brd.(i).(j) in
    let _, label = board_matrix.(i).(j) in
    W.set_text label letter
  in

  let relabel_tiles brd : unit =
    for i = 0 to 3 do
      for j = 0 to 3 do
        assign_label_letter i j brd
      done
    done
  in

  let timer_label = W.label ~size:40 "Time Left: " in

  let board_solutions brd =
    let hashtable = Hashtbl.create 10 in
    GameBoard.solve brd hashtable;
    GameBoard.solutions hashtable
  in

  let top_possible_words_display =
    W.text_display ~w:100 ~h:630
      (convert_str_lst_to_str
         (GameBoard.longest_words (board_solutions !board) 20))
  in

  let display_top_possible_words () =
    let top_possible_words =
      GameBoard.longest_words (board_solutions !board) 20
    in
    W.set_text top_possible_words_display
      (convert_str_lst_to_str top_possible_words)
  in

  let restart_game () =
    board := GameBoard.new_board ();
    reset_tiles board_matrix;
    W.set_text word_field "";
    (* set_score 0; *)
    relabel_tiles !board;
    display_top_possible_words ()
  in

  let restart_button =
    let action _ = restart_game () in
    W.button ~action ~kind:Button.Trigger "Start New Game"
  in
  let restart_button_flat =
    L.flat ~margins:100 [ L.resident ~w:(width * 2) restart_button ]
  in

  let page2_first_row =
    L.flat ~hmargin:50
      [
        L.resident ~w:width ~h:60 word_field;
        L.resident ~w:250 ~h:60 score_board;
        L.resident ~w:300 ~h:60 timer_label;
      ]
  in

  let page2_second_row = L.flat [ game_board; used_words_tower ] in

  let page2 =
    L.tower
      [
        page2_first_row;
        page2_second_row;
        enter_button_flat;
        restart_button_flat;
      ]
  in

  (*Page 3 *********************************************************************)

  (* TODO: for some reason when this code is used instead of the dup before
     the function above, the text area gets too short *)
  (* let () = display_top_possible_words () in *)
  let top_possible_words_title =
    W.rich_text ~size:20 ~w:width ~h:30
      Text_display.(page [ bold (para "Longest Possible Words") ])
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

  let top_user_word_list =
    L.tower
      [ L.resident top_user_words_title; L.resident top_user_words_display ]
  in

  let display_lists =
    L.flat ~hmargin:200 [ top_words_list; top_user_word_list ]
  in

  let back_to_page2_button = W.button "Back to Board" in

  let page3 =
    let message = W.label ~size:50 " Thanks for playing!" in

    L.tower
      [
        L.resident ~w:1000 ~h:70 message;
        display_lists;
        L.resident ~w:750 ~h:50 back_to_page2_button;
        L.resident ~w:1000 ~h:50 score_message;
      ]
  in

  (* extra stuff ***************************************************************)
  let use_tabs = false in
  (*let page2 = L.tower [ layout ] in*)
  let tabs =
    if use_tabs then
      Tabs.create ~slide:Avar.Right
        [ ("Page 1", page1); ("Page 2", page2); ("Page 3", page3) ]
    else L.superpose [ page1; page2; page3 ]
  in

  (* Useful *)
  (* print_endline (Print.layout_down tabs); *)

  (* zero-based *)
  let switch_mode n =
    if use_tabs then ()
      (* TODO: make this less fragile *)
      (*
             let top_rooms = L.get_rooms tabs in
             match L.get_rooms (List.hd top_rooms) with
             | [ t1; t2; t3 ] ->
                 L.set_show t1 (n = 0);
                 L.set_show t2 (n = 1);
                 L.set_show t3 (n = 2)
             | _ -> failwith "Unexpected layout in switch_tab!" *)
    else (
      print_endline ("switch_mode: " ^ string_of_int n);
      L.set_show page1 (n = 0);
      L.set_show page2 (n = 1);
      L.set_show page3 (n = 2))
  in

  let _ = switch_mode 0 in

  let update_timer_normal n =
    let rec loop remaining_time =
      if remaining_time >= 0 then (
        W.set_text timer_label ("Time Left: " ^ string_of_int remaining_time);
        Unix.sleep 1;
        W.update timer_label;
        loop (remaining_time - 1))
      else (
        W.set_text timer_label "Time is up";
        W.update timer_label;
        switch_mode 2)
    in
    ignore (Thread.create loop n)
  in

  let update_timer_survival n =
    let rec loop remaining_time =
      if remaining_time >= 0 then (
        match !has_entered_word with
        | true, false, n ->
            toggle_has_entered_word_timer ();
            let new_remaining_time = remaining_time + n in
            W.set_text timer_label
              ("Time Left: " ^ string_of_int (new_remaining_time + n));
            W.update timer_label;
            Unix.sleep 1;
            loop (new_remaining_time - 1)
        | _ ->
            W.set_text timer_label ("Time Left: " ^ string_of_int remaining_time);
            W.update timer_label;
            Unix.sleep 1;
            loop (remaining_time - 1))
      else (
        W.set_text timer_label "Time is up";
        W.update timer_label;
        switch_mode 2)
    in
    ignore (Thread.create loop n)
  in

  let start_button_normal_action input label _ =
    switch_mode 1;
    update_timer_normal 60
  in

  let start_button_survival_action input label _ =
    switch_mode 1;
    update_timer_survival 20
  in

  let c1 =
    W.connect start_normal_button welcome_label start_button_normal_action
      Sdl.Event.[ mouse_button_down ]
  in

  let c2 =
    W.connect start_survival_button welcome_label start_button_survival_action
      Sdl.Event.[ mouse_button_down ]
  in

  let back_to_page2_action _ _ _ = switch_mode 0 in

  let c3 =
    W.connect back_to_page2_button back_to_page2_button back_to_page2_action
      Sdl.Event.[ mouse_button_down ]
  in

  let board = Bogue.of_layout ~connections:[ c1; c2; c3 ] tabs in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
