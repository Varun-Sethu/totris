type t = (cell Array.t) Array.t
  and cell =
      | Empty
      | Red | Green | Blue | Yellow

let empty_board ~height ~width = Array.init height (fun _ -> Array.init width (fun _ -> Empty))


let colour_to_cell = function
  | Piece.Red -> Red
  | Piece.Blue -> Blue
  | Piece.Green -> Green
  | Piece.Yellow -> Yellow


let dimensions ~board = Array.length board.(0), Array.length board


let get_cell_at ~x ~y ~board = board.(y).(x)


(* piece_can_be_placed determines if the piece is colliding with any other elements on the board and is within the bounds of the board*)
let piece_can_be_placed ~piece ~position ~board =
  let (width, height)   = dimensions ~board:board in 
  let piece_abs_coords  = Piece.get_abs_coords ~origin:position piece in
  let piece_is_in_board = Array.for_all (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height) piece_abs_coords in

  if piece_is_in_board then
    let piece_cells_are_empty = Array.for_all (fun (x, y) -> get_cell_at ~x:x ~y:y ~board:board = Empty) piece_abs_coords in
    piece_cells_are_empty
  else
    false


let clear_rows board =
  let (width, height) = dimensions ~board:board in
  let row_is_full     = Array.for_all (fun cell -> cell <> Empty) in

  (* copy the non-full rows from the current board to the new board *)
  let rec copy_partial_rows row_to_copy =
    if row_to_copy < 0 then [||]
    else begin 
      if row_is_full board.(row_to_copy) then copy_partial_rows (row_to_copy - 1)
      else 
        Array.append (copy_partial_rows (row_to_copy - 1)) [| (Array.copy board.(row_to_copy)) |]
    end in
  
  (* pad out the copied array until it reaches the required height *)
  let rec pad_board board = 
    if Array.length board < height then
      pad_board (Array.append [| (Array.init width (fun _ -> Empty)) |] board)
    else board in
  
  copy_partial_rows (height - 1)
    |> pad_board


(* place_piece physically drops a piece at the specified position, it returns None if the piece would obstruct something *)
let place_piece ~piece ~position ~board =
  if piece_can_be_placed ~piece:piece ~position:position ~board:board then
    let piece_abs_coords = Piece.get_abs_coords ~origin:position piece in
    Array.iter (fun (x, y) -> (board.(y).(x) <- colour_to_cell piece.colour)) piece_abs_coords;
    Some board
  else
    None


let place_player ~piece ~position ~board =
  (* since playes move around a lot we want to duplicate the board instead of modifying it directly *)
  let new_board = Array.map (Array.copy) board in
  place_piece ~piece:piece ~position:position ~board:new_board