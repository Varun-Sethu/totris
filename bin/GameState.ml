type gameState = {
  current_piece: Piece.t;
  current_board: Board.t;
  current_player_pos: (int * int)
}

type movementDirection = Left | Right | Down

let get_random_piece () =
  let piece_generators = Array.of_list [
    (fun () -> Piece.create_piece ([| [|(0, 0); (0, 1)|]; [|(0, 0); (0, 1)|] |]) Piece.Green);
    (fun () -> Piece.create_piece ([| [|(0, 0); (0, 1)|]; [|(0, 0); (0, 1)|] |]) Piece.Blue)
  ] in

  let generator = piece_generators.(Random.int (Array.length piece_generators)) 
  in generator ()


let init_state ~height ~width = {
  current_piece = get_random_piece ();
  current_board = Board.empty_board ~height:height ~width:width;
  current_player_pos = (width / 2, 0);
}


let reset_state ~game_state =
  let new_piece = get_random_piece () in
  let (width, _) = Board.dimensions ~board:game_state.current_board in
  {
    current_piece = new_piece;
    current_board = Board.place_piece ~piece:game_state.current_piece ~position:game_state.current_player_pos ~board:game_state.current_board |> Option.get;
    current_player_pos = (width / 2, 0)
  }


let move_player ~direction ~game_state = 
  let new_position = 
    match (direction, game_state.current_player_pos) with
      | (Left,  (x, y)) -> (x - 1, y)
      | (Right, (x, y)) -> (x + 1, y)
      | (Down,  (x, y)) -> (x, y + 1) in
  
  if Board.piece_can_be_placed ~piece:game_state.current_piece ~position:new_position ~board:game_state.current_board then
    Some { current_piece = game_state.current_piece; current_board = game_state.current_board; current_player_pos = new_position }
  else
    None

(* board_with_player returns the game board for the game state with the current player rendered *)
let board_with_player ~game_state = 
  Board.place_player ~piece:game_state.current_piece ~position:game_state.current_player_pos ~board:game_state.current_board
    |> Option.get


(* ticks the game by one timestep *)
let timestep ~game_state =
  match move_player ~direction:Down ~game_state:game_state with
    | Some (game_state) -> game_state
    | None -> reset_state ~game_state:game_state