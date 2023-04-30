open Notty.I (* images *)
open Notty.A (* attributes *)
open Notty_unix

(* render_full_screen renders the full tetris screen given how it should render
   the boarder and the actual game *)
let render_full_screen ~border ~game = 
  let (width, height) = (12, 22) in
  let draw_screen x y =
    match (x, y) with
      | (x, y) when x = 0 || y = 0 || x = width - 1 || y = height - 1 -> border (x, y) 
      | _ -> game (x, y)
  in
    tabulate width height draw_screen

(* render_game_board renders the actual part of the screen that contains the 
   actual tetris game*)
let render_game_board (_, _) = string (fg magenta ++ bg magenta) "\xe2\x96\xaa\xe2\x96\xaa"

(* render_border renders the physical border around the game*)
let render_border (_, _) = string (fg (gray 10)) ("<>")

let x = Board.empty_board ~height:30 ~width:20


let () = 
  output_image (render_full_screen ~border:render_border ~game:render_game_board);
  Array.iter (fun _ -> ()) x;
  print_string "\n"