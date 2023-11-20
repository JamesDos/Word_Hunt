open Bogue
module W = Widget
module L = Layout
module GameBoard = Builder.BuildBoard

let board = GameBoard.game_board

let main () =
  let a = board in
  let headers = [ "Column 1"; "Column 2"; "Column 3"; "Column 4" ] in
  let widths = [ Some 100; Some 100; Some 100; Some 100 ] in
  let table, _ = Table.of_array ~h:400 ~widths headers a in

  let layout = L.tower [ L.resident (W.label "Word Hunt"); table ] in

  let board = Bogue.of_layout layout in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
