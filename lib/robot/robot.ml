open! Core

type t = Shapes.t
   

let state_create () : t =
  Shapes.create 100000

let draw_and_update (t : t) _loop_state =
  Shapes.draw t

let start () =
  Main_loop.loop_draw
    ~draw:draw_and_update
    ~state_f:state_create
