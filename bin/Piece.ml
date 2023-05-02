type piece_colour = Red | Green | Blue | Yellow
type t = {
  block_positions: (int * int) array;
  colour: piece_colour;
}

let create_piece configuration colour = {
  block_positions = configuration;
  colour = colour;
}

(* apply rotation matrix to each tuple *)
(* the first coordinate  is treated as the origin of the rotation *)
let rotate_piece piece =
  let (o_x, o_y) = piece.block_positions.(0) in
  let rotated_pieces = 
    piece.block_positions  
      |> Array.map (fun (x, y) -> (x - o_x, y - o_y)) (* compute relative positions from origin *)
      |> Array.map (fun (x, y) -> (-y, x)) (* rotate around origin *)
      |> Array.map (fun (x, y) -> (x + o_x, y + o_y)) in (* move back to origin *)
  { piece with block_positions = rotated_pieces }


let get_abs_coords ~origin piece =
  let (origin_x, origin_y) = origin in
  Array.map (fun (x, y) -> (origin_x + x, origin_y + y)) piece.block_positions