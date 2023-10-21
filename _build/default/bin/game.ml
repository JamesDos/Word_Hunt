open Builder

let board = Array.make_matrix 4 4 'a'
let game_board = BuildBoard.fill_board board
let () = BuildBoard.print_board game_board
