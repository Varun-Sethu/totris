type t = (cell Array.t) Array.t
  and cell =
      | Empty
      | Red | Green | Blue | Yellow

  let empty_board ~height ~width = Array.init width (fun _ -> Array.init height (fun _ -> Empty))

  (* is_colliding determines if the piece is colliding with any other elements on the board *)
  let is_colliding ~piece ~position ~board =
    let piece_abs_coords = Piece.get_abs_coords ~origin:position piece in
    let contained_cells_are_emtpy = Array.for_all (fun (x, y) -> board.(x).(y) = Empty) piece_abs_coords in
    not contained_cells_are_emtpy

    (* place_piece physically drops a piece at the specified position, it returns None if the piece would obstruct something *)
  let place_piece ~piece ~color ~position ~(board: t) =
    if is_colliding ~piece:piece ~position:position ~board:board then
      None
    else
      let piece_abs_coords = Piece.get_abs_coords ~origin:position piece in
      Array.iter (fun (x, y) -> (board.(x).(y) <- color)) piece_abs_coords;
      Some board
  
  let place_player ~(piece: Piece.t) ~color ~(position: int * int) ~(board: t) = board