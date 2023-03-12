open! Core

type t = Color_wheels.t

let create (loop_state : Main_loop.t) =
  Color_wheels.create loop_state


let draw_and_update (t : t) (loop_state : Main_loop.t) =
  Color_wheels.draw_and_update t loop_state

let start () =
  Main_loop.loop_draw
    ~draw:draw_and_update
    ~state_f:create
