type t = {
  current_piece: Piece.t;
  current_board: Board.t;
  current_player_pos: (int * int)
}

type movementDirection = Left | Right | Down

let get_random_piece () =
  Random.self_init ();
  let piece_generators = Array.of_list [
    (fun () -> Piece.create_piece ~configuration:([|(0, 0); (0, 1); (0, 2); (0, 3) |]) ~colour:Colour.Blue); (* flat piece *)
    (fun () -> Piece.create_piece ~configuration:([|(0, 0); (1, 0); (1, 1); (1, 2) |]) ~colour:Colour.Yellow); (* l piece *)
    (fun () -> Piece.create_piece ~configuration:([|(0, 0); (0, 1); (1, 0); (1, 1) |]) ~colour:Colour.Green); (* square piece *)
    (fun () -> Piece.create_piece ~configuration:([|(1, 0); (1, 1); (0, 1); (2, 1) |]) ~colour:Colour.Red) (* weird t piece *)
  ] in

  let generator = piece_generators.(Random.int (Array.length piece_generators)) 
  in generator ()

let init_state ~height ~width = {
  current_piece = get_random_piece ();
  current_board = Board.empty_board ~height ~width;
  current_player_pos = (width / 2, 0);
}

let reset_state ~game_state =
  let (width, _) = Board.dimensions ~board:game_state.current_board in
  let new_piece = get_random_piece () in
  let new_board = Board.place_piece ~piece:game_state.current_piece ~position:game_state.current_player_pos ~board:game_state.current_board 
                    |> Option.map (fun board -> (Board.clear_full_rows ~board)) in

  (* todo: update to use ppx monadic do blocks *)
  Option.map (fun new_board -> {
    current_piece = new_piece;
    current_board = new_board;
    current_player_pos = (width / 2, 0)
  }) new_board

let move_player ~game_state ~direction = 
  let new_position = 
    match (direction, game_state.current_player_pos) with
      | (Left,  (x, y)) -> (x - 1, y)
      | (Right, (x, y)) -> (x + 1, y)
      | (Down,  (x, y)) -> (x, y + 1) in

  if not (Board.piece_can_be_placed ~piece:game_state.current_piece ~position:new_position ~board:game_state.current_board) then None
  else
    Some { game_state with current_player_pos = new_position }

let rotate_player ~game_state =
  let rotated_piece = Piece.rotate ~piece:game_state.current_piece in
  if not (Board.piece_can_be_placed ~piece:rotated_piece ~position:game_state.current_player_pos ~board:game_state.current_board) then None
  else
    Some { game_state with current_piece = Piece.rotate ~piece:game_state.current_piece }

let get_board ~game_state =
  Board.place_player 
    ~board:game_state.current_board 
    ~player_piece:game_state.current_piece 
    ~position:game_state.current_player_pos

let timestep ~game_state =
  match move_player ~direction:Down ~game_state with
    | Some game_state -> Some game_state
    | None -> reset_state ~game_state

let rec drop_current_piece ~game_state = 
  match move_player ~game_state ~direction:Down with
    | Some new_game_state -> drop_current_piece ~game_state:new_game_state
    | None -> timestep ~game_state (* drop the piece (prevents weird buggy screen issues) *)