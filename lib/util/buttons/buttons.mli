open! Core
open Mat_vec

module Button : sig
  type t = {
      mutable position : Vec.t;
      width : Float.t;
      height : Float.t;
      press_f : int -> (Vec.t * int);
      release_f : int -> (Vec.t * int);
      mutable state : int;
      mutable color_state : Vec.t;
      mutable pressed : bool;
      mutable label : string;
      mutable text_size : float;
      mutable label_pos : Vec.t
    }

  val state : t -> int

  val update_state : t -> (int * Vec.t) -> unit
end
   
type t = {
    mutable buttons : Button.t list;
    mutable selected : Button.t option;
    mutable checked_last_click : bool
  }

val create : unit -> t

val add_button : t -> Vec.t -> width:Float.t -> height:Float.t -> (int -> (Vec.t * int)) -> (int -> (Vec.t * int)) -> int -> Vec.t -> string -> float -> Vec.t -> Button.t

val draw_and_update : t -> Mouse.t -> Draw_primitives.t
                      -> unit

