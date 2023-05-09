open Notty_lwt
open Notty
open Lwt.Infix


let block_timer () = Lwt_unix.sleep 0.9 >|= fun () -> `Timer
let gui_event t = Lwt_stream.get (Term.events t) >|= 
  function
    | Some (`Resize _ | #Unescape.event as x) -> x
    | None -> `End


let () =
  let t = Term.create () in
  let game_state = ref (GameState.init_state ~height:20 ~width:10) in
  let render_screen () =
    let new_screen = Renderer.render_screen_with_board ~board:(GameState.get_board ~game_state:!game_state) in
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
    ((block_timer () <?> gui_event t) >>= 
      function
        | `Key (`Arrow `Right, _) -> update_state ~transformer:(GameState.move_player ~direction:GameState.Right)
        | `Key (`Arrow `Left, _)  -> update_state ~transformer:(GameState.move_player ~direction:GameState.Left)
        | `Key (`Arrow `Down, _)  -> update_state ~transformer:(fun ~game_state -> GameState.timestep ~game_state |> Option.some)
        | `Key (`Arrow `Up, _)    -> update_state ~transformer:(GameState.rotate_player)
        | `Timer                  -> update_state ~transformer:(fun ~game_state -> GameState.timestep ~game_state |> Option.some)
        | _                       -> Lwt.return_unit)
  in
  Lwt_main.run @@ game_loop ()