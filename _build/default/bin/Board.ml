type t = (cell Array.t) Array.t
  and cell =
      | Empty
      | Red | Green | Blue | Yellow

  let empty_board ~height ~width = Array.init width (fun _ -> Array.init height (fun _ -> Empty))
  let place_piece ~(piece: Piece.t) ~(position: int * int) ~(board: t) = board
  let render_player_piece ~(piece: Piece.t) ~(position: int * int) ~(board: t) = board