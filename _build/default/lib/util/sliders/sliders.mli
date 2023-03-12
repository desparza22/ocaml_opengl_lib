open! Core

module Slider_dims : sig
  type t = {
      mutable button_height : Float.t;
      mutable button_width : Float.t;
      mutable center_y : Float.t;
      mutable left : Float.t;
      mutable right : Float.t;
      mutable label : string;
      mutable text_size : Float.t;
      mutable label_pos : float Array.t;
      mutable value_pos : float Array.t
    }
end

module Slider : sig

  type t = {
      min : Float.t;
      max : Float.t;
      mutable value : Float.t;
      mutable active : bool;
      dims : Slider_dims.t
    }

  val value : t -> Float.t

  val update_value : t -> Float.t -> unit

  val set_active : t -> bool -> unit
end

type t = {
    mutable num_sliders : int;
    max_sliders : int;
    sliders : Slider.t array;
    mutable selected : int option;
    mutable checked_last_click : bool
  }

val create : unit -> t

val add_slider :
  t -> min:Float.t -> max:Float.t -> value:Float.t -> Slider_dims.t -> Slider.t

val draw_and_update : t -> Mouse.t -> Draw_primitives.t
                      -> unit
