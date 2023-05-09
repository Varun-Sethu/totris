type t
type movementDirection = Left | Right | Down

val init_state: height: int -> width: int -> t

val move_player: game_state: t -> direction: movementDirection -> t option
val rotate_player: game_state: t -> t option
val drop_current_piece: game_state: t -> t option
val timestep: game_state: t -> t option


val get_board: game_state: t -> Board.t option