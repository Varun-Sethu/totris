type t
type cell = 
    | Empty
    | Filled of Colour.t

(* empty_board returns a brand new empty board of the specified width and height *)
val empty_board: width: int -> height: int -> t

(* dimensions computes the dimensions for a given board returned in (width, height) order *)
val dimensions: board: t -> (int * int)

(* piece_can_be_placed determines if a piece can be placed on the board at the specified (x, y) coords *)
val piece_can_be_placed: board: t -> piece: Piece.t -> position: (int * int) -> bool

(* clear_rows deletes all full rows from the board *)
val clear_full_rows: board: t -> t

(* place_piece places an actual piece at the specified location, returns None if cant be placed *)
val place_piece: board: t -> piece: Piece.t -> position: (int * int) -> t Option.t

(* get_cell_at returns the cell at the specified location *)
val get_cell_at: board: t -> x: int -> y: int -> cell

(* place_player places an actual player piece at the specified location, returns None if cant be placed *)
val place_player: board: t -> player_piece: Piece.t -> position: (int * int) -> t Option.t