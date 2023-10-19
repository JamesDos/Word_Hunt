open Bogue
module W = Widget
module L = Layout

let main () =
  let border =
    W.box ~w:500 ~h:500
      ~style:(Style.of_border (Style.mk_border (Style.mk_line ())))
      ()
  in
  let layout = L.tower_of_w [ border ] in
  let game_board =
    Table.of_array ~w:100 ~h:100 [ "b"; "b"; "b"; "b" ]
      (Array.make_matrix 3 4 "b")
  in
  let window =
    L.flat
      ~background:(L.color_bg (Draw.opaque (95, 214, 95)))
      [ layout; fst game_board ]
  in
  let board = Bogue.of_layout window in
  Bogue.run board

let () =
  main ();
  Bogue.quit ()
