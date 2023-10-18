(*Testing*)
(*
open Bogue
module W = Widget
module L = Layout

let main () =
  let b = W.check_box () in
  let l = W.label "Hello world" in
  let layout = L.flat_of_w [ b; l ] in

  let board = Bogue.of_layout layout in
  Bogue.run board

let () =
  main ();
  Bogue.quit () *)

let random_char () =
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  chars.[Random.int (String.length chars)]

let board = Array.make_matrix 4 4 'a'

let fill_board array_2d =
  let rows = Array.length array_2d in
  let cols = Array.length array_2d.(0) in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      array_2d.(i).(j) <- random_char ()
    done
  done;
  array_2d

let game_board = fill_board board

let print_board array_2d =
  let rows = Array.length array_2d in
  let cols = Array.length array_2d.(0) in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      Printf.printf "%c " array_2d.(i).(j)
    done;
    print_newline ()
  done

let () = print_board game_board
