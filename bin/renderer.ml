open Notty

(* render_full_screen renders the full tetris screen given how it should render
  the boarder and the actual game *)
let render_full_screen ~border ~game = 
 let (width, height) = (12, 22) in
 let draw_screen x y =
   match (x, y) with
     | (x, y) when x = 0 || y = 0 || x = width - 1 || y = height - 1 -> border (x, y) 
     | _ -> game (x - 1, y - 1)
 in
   I.tabulate width height draw_screen

(* render_game_board renders the actual part of the screen that contains the 
   actual tetris game*)
let render_game_board game_board (x, y) =
 let open Notty.A in
 let open Notty.I in
 let square = "\xe2\x96\xaa\xe2\x96\xaa" in
 match Board.get_cell_at ~x:x ~y:y ~board:game_board with
   | Board.Empty   -> void 2 0
   | Board.Filled (Colour.Green)   -> string (fg green ++ bg green) square
   | Board.Filled (Colour.Blue)    -> string (fg blue ++ bg blue) square
   | Board.Filled (Colour.Yellow)  -> string (fg yellow ++ bg yellow) square
   | Board.Filled (Colour.Red)     -> string (fg red ++ bg red) square

(* render_border renders the physical border around the game*)
let render_border (_, _) = Notty.I.string Notty.A.(fg (gray 10)) ("<>")

let render_screen_with_board ~board =
  render_full_screen 
    ~border:render_border 
    ~game:(render_game_board board)