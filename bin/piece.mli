type t

val create_piece: configuration: (int * int) array -> colour: Colour.t -> t

(* rotate rotates a provided piece by 90 degrees *)
val rotate: piece: t -> t

(* get_abs_coords projects a piece onto a coordinate system with the provided origin *)
val get_abs_coords: piece: t -> origin: (int * int) -> (int * int) array

(* get_colour returns the colour of a piece *)
val get_colour: piece: t -> Colour.t
