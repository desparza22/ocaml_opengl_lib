open! Core
open Mat_vec

module Dragger : sig  
  module Draw_context : sig
    type t = {
        time : Float.t;
        position : Vec.t;
        active : bool
      }
  end
  
  type t = {
      mutable position : Vec.t;
      mutable active : bool;
      mutable press_f : unit -> unit;
      mutable hold_f : (Float.t * Float.t) -> unit;
      mutable draw_fun :
                Draw_context.t -> Draw_primitives.t -> unit;
      mutable matches_xy :
                t -> x:float -> y:float -> bool
    }
         
  val get_pos : t -> Vec.t

  val update_pos : t -> Vec.t -> unit

  val set_active : t -> bool -> unit

  val set_press_f : t -> (unit -> unit) -> unit

  val set_hold_f : t -> ((Float.t * Float.t) -> unit) ->
                   unit

  val set_draw_fun : t ->
                     (Draw_context.t ->
                      Draw_primitives.t -> unit) ->
                     unit

  val set_matches_xy : t ->
                       (t -> x:float -> y:float -> bool) ->
                       unit

end

type t = {
    mutable draggers : Dragger.t list;
    mutable selected : Dragger.t option;
    mutable checked_last_click : bool;
    mutable time : Float.t
  }

val create : unit -> t

val add_dragger : t -> Vec.t -> press_f:(unit -> unit) option -> hold_f:((Float.t * Float.t) -> unit) option -> Dragger.t

val draw_and_update : t -> Mouse.t -> Draw_primitives.t ->
                      unit
