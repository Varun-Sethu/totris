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
      | _ -> game (x - 1, y - 1)
  in
    tabulate width height draw_screen

(* render_game_board renders the actual part of the screen that contains the 
   actual tetris game*)
let render_game_board game_board (x, y) =
  let square = "\xe2\x96\xaa\xe2\x96\xaa" in
  match Board.get_cell_at ~x:x ~y:y ~board:game_board with
    | Board.Empty   -> void 2 0
    | Board.Green   -> string (fg green ++ bg green) square
    | Board.Blue    -> string (fg blue ++ bg blue) square
    | Board.Yellow  -> string (fg yellow ++ bg yellow) square
    | Board.Red     -> string (fg red ++ bg red) square

(* render_border renders the physical border around the game*)
let render_border (_, _) = string (fg (gray 10)) ("<>")


let () =
  let t = Term.create () in
  let game_state = ref (GameState.init_state ~height:20 ~width:10) in
  let render_screen () =
    Term.image t (
      render_full_screen 
        ~border:render_border 
        ~game:(render_game_board (GameState.board_with_player ~game_state:!game_state))) in

  let rec game_loop () = 
    match Term.event t with
      | `Key (`Enter,_)        -> ()
      | _                      ->
        game_state := (GameState.timestep ~game_state:!game_state);
        Term.refresh t;
        render_screen ();
        game_loop (); in

  game_loop ();
  print_string "\n"