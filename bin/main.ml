open Notty.I (* images *)
open Notty.A (* attributes *)
open Notty_lwt
open Notty
open Notty.Infix
open Lwt.Infix

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

let block_timer () = Lwt_unix.sleep 0.9 >|= fun () -> `Timer
let gui_event t = Lwt_stream.get (Term.events t) >|= 
  function
    | Some (`Resize _ | #Unescape.event as x) -> x
    | None -> `End


let () =
  let t = Term.create () in
  let game_state = ref (GameState.init_state ~height:20 ~width:10) in
  let render_screen () =
    let new_screen = render_full_screen 
                      ~border:render_border 
                      ~game:(render_game_board (GameState.board_with_player ~game_state:!game_state)) in

    Term.refresh t >>=
      fun () -> Term.image t new_screen 
    in

  (* core game loop logic *)
  let rec update_state ~transformer =
    let new_state = match transformer ~game_state:!game_state with
                      | Some new_state -> new_state
                      | None -> !game_state in
    game_state := new_state;
    render_screen () >>= game_loop

  and game_loop () =
    ((gui_event t <?> block_timer ()) >>= 
      function
        | `Key (`Arrow `Right, _) -> update_state ~transformer:(GameState.move_player ~direction:GameState.Right)
        | `Key (`Arrow `Left, _)  -> update_state ~transformer:(GameState.move_player ~direction:GameState.Left)
        | `Key (`Arrow `Down, _)  -> update_state ~transformer:(fun ~game_state -> GameState.timestep ~game_state |> Option.some)
        | `Key (`Arrow `Up, _)    -> update_state ~transformer:(GameState.rotate_player)
        | `Timer                  -> update_state ~transformer:(fun ~game_state -> GameState.timestep ~game_state |> Option.some)
        | _                       -> Lwt.return_unit)
  in
  Lwt_main.run @@ game_loop ()