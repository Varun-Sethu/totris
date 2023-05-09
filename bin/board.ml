type t = (cell Array.t) Array.t
  and cell =
      | Empty
      | Filled of Colour.t
      
let empty_board ~width ~height = Array.init height (fun _ -> Array.init width (fun _ -> Empty))
let dimensions ~board = Array.length board.(0), Array.length board
let get_cell_at ~board ~x ~y = board.(y).(x)

let piece_can_be_placed ~board ~piece ~position =
  let (width, height)   = dimensions ~board in 
  let piece_abs_coords  = Piece.get_abs_coords ~origin:position ~piece in
  let piece_is_in_bounds = Array.for_all (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height) piece_abs_coords in

  if not piece_is_in_bounds then false
  else
    let piece_isnt_colliding = Array.for_all (fun (x, y) -> get_cell_at ~x ~y ~board = Empty) piece_abs_coords in
    piece_isnt_colliding

let place_piece ~board ~piece ~position =
  if not (piece_can_be_placed ~piece ~position ~board) then None
  else
    let piece_abs_coords = Piece.get_abs_coords ~origin:position ~piece in
    let board_with_piece = Array.fold_left (fun board (x, y) -> board.(y).(x) <- Filled (Piece.get_colour ~piece); board) board piece_abs_coords in (* dont need to fold but look nice :D *)
    Some board_with_piece

let place_player ~(board: t) ~player_piece ~position =
  let cloned_board = Array.map (Array.copy) board in
  place_piece ~piece:player_piece ~position ~board:cloned_board



(* delete_full_rows copies the non-full rows from the current board to a brand board *)
let delete_full_rows ~board = 
  let (_, height) = dimensions ~board in

  let row_is_full ~row = Array.for_all (fun cell -> cell <> Empty) row in
  let rec copy_over_incomplete_rows candidate_row =
    if candidate_row < 0 then [||]
    else
      if row_is_full ~row:board.(candidate_row) then copy_over_incomplete_rows (candidate_row - 1)
      else
        let rest_of_board  = copy_over_incomplete_rows (candidate_row - 1) in
        let incomplete_row = (Array.copy board.(candidate_row)) in
        Array.append rest_of_board [| incomplete_row |] in 
  
  copy_over_incomplete_rows (height - 1)

(* pad_board_to_height adds rows to the top of a board until it is a specified height *)
let rec pad_board_to_height ~board ~target_height = 
  let (width, height) = dimensions ~board in
  if height >= target_height then board
  else
    pad_board_to_height ~board:(Array.append [| (Array.init width (fun _ -> Empty)) |] board) ~target_height



let clear_full_rows ~board =
  let (_, height)  = dimensions ~board in
  let packed_incomplete_rows = delete_full_rows ~board in
  let padded_board = pad_board_to_height ~board:packed_incomplete_rows ~target_height:height in 
  
  padded_board