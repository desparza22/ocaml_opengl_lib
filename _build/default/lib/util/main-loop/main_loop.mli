open! Core

type t =
  {
    mutable mouse : Mouse.t;
    draw_primitives : Draw_primitives.t;
    sliders : Sliders.t;
    draggers : Draggers.t;
    buttons : Buttons.t
  }

val delete_program : int -> (unit, _) Result.t

val reshape : GLFW.window -> int -> int -> unit

val loop_draw : draw:('a -> t -> unit) ->
                state_f:(t -> 'a) -> unit
