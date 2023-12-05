open Bogue
open Tsdl
module W = Widget
module L = Layout
module T = Trigger
module GameBoard = Builder.BuildBoard

let board = GameBoard.game_board

let main () =
  let make_tile letter = W.button ~kind:Button.Switch letter in

  let rec make_rowi acc i j =
    if j = 0 then acc
    else make_rowi (L.resident (make_tile board.(i).(j)) :: acc) i (j - 1)
  in

  let board_row1 = make_rowi [] 0 3 |> L.flat in
  let board_row2 = make_rowi [] 1 3 |> L.flat in
  let board_row3 = make_rowi [] 2 3 |> L.flat in
  let board_row4 = make_rowi [] 3 3 |> L.flat in
  let game_board = L.tower [ board_row1; board_row2; board_row3; board_row4 ] in

  let a = board in
  let headers = [ "Column 1"; "Column 2"; "Column 3"; "Column 4" ] in
  let widths = [ Some 100; Some 100; Some 100; Some 100 ] in
  let table, _ = Table.of_array ~h:400 ~widths headers a in

  let action input label _ =
    let text = W.get_text input in
    W.set_text label ("Hello " ^ text ^ "!")
  in

  let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in
  let label = W.label ~size:40 "Hello!" in

  let c = W.connect input label action Sdl.Event.[ text_input; key_down ] in

  let layout =
    L.tower [ L.resident ~w:400 input; L.resident ~w:400 ~h:200 label ]
  in

  let page1 = L.tower [ layout ] in
  let page2 = L.tower [ game_board ] in

  (*let page2 = L.tower [ layout ] in*)
  let tabs =
    Tabs.create ~slide:Avar.Right [ ("Page 1", page1); ("Page 1", page2) ]
  in
  (*let layout = L.tower [ L.resident (W.label "Word Hunt"); table ] in*)
  let board = Bogue.of_layout ~connections:[ c ] tabs in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
