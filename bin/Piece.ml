type piece_colour = Red | Green | Blue | Yellow
type t = {
  orientations: ((int * int) array) array;
  current_orientation: int;
  colour: piece_colour;
}

let create_piece orientations colour = {
  orientations = orientations;
  current_orientation = 0;
  colour = colour;
}

let rotate_piece piece =
  let next_orientation = (piece.current_orientation + 1) mod Array.length piece.orientations in
  {
    orientations = piece.orientations;
    current_orientation = next_orientation;
    colour = piece.colour;  
  }

let get_abs_coords ~origin piece =
  let current_orientation = piece.orientations.(piece.current_orientation) in
  let (origin_x, origin_y) = origin in

  Array.map (fun (x, y) -> (origin_x + x, origin_y + y)) current_orientation