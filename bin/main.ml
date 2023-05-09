open Notty_lwt
open Notty
open Lwt.Infix

let block_timer_listener () = Lwt_unix.sleep 0.9 >|= fun () -> `Timer
let gui_event_listener t = Lwt_stream.get (Term.events t) >|= 
  function
    | Some (`Resize _ | #Unescape.event as x) -> x
    | None -> `End


let () =
  let t = Term.create () in
  let game_state = ref (GameState.init_state ~height:20 ~width:10) in

  (* if the board is none (cant be rendered anymore) then stop the game *)
  let rec rerender_screen ~block_timer ~gui_event =
    let new_screen = match GameState.get_board ~game_state:!game_state with
      | None -> Renderer.render_game_over
      | Some board -> Renderer.render_screen_with_board ~board in

    Term.refresh t 
      >>= fun () -> Term.image t new_screen
      >>= fun () -> game_loop ~block_timer ~gui_event

  (* core game loop logic *)
  and update_state ~transformer ~block_timer ~gui_event =
    let new_state = match transformer ~game_state:!game_state with
                      | Some new_state -> new_state
                      | None -> !game_state in
    game_state := new_state;
    rerender_screen ~block_timer ~gui_event

  and game_loop ~block_timer ~gui_event =
    ((block_timer <?> gui_event) >>= 
      function
        | `Key (`Arrow `Right, _) -> update_state ~transformer:(GameState.move_player ~direction:GameState.Right) ~block_timer ~gui_event:(gui_event_listener t)
        | `Key (`Arrow `Left, _)  -> update_state ~transformer:(GameState.move_player ~direction:GameState.Left) ~block_timer ~gui_event:(gui_event_listener t)
        | `Key (`Arrow `Down, _)  -> update_state ~transformer:(GameState.move_player ~direction:GameState.Down) ~block_timer ~gui_event:(gui_event_listener t)
        | `Key (`Arrow `Up, _)    -> update_state ~transformer:(GameState.rotate_player) ~block_timer ~gui_event:(gui_event_listener t)
        | `Key (`ASCII ' ', _)    -> update_state ~transformer:(GameState.drop_current_piece) ~block_timer ~gui_event:(gui_event_listener t)
        | `Timer                  -> update_state ~transformer:(GameState.timestep) ~block_timer:(block_timer_listener ()) ~gui_event
        | _                       -> Lwt.return_unit) in

  Lwt_main.run @@ rerender_screen ~block_timer:(block_timer_listener ()) ~gui_event:(gui_event_listener t)