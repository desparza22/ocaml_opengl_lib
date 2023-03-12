open! Mat_vec
open! Core

module Circle : sig
  type t = {
      center : Vec.t;
      radius : Float.t
    }

  val theta_of_point : t -> Vec.t -> Float.t

  val point_along : t -> Float.t -> Vec.t

  val intersects : t -> x:float -> y:float -> bool

end


type t = Draw_container.t

val create : string -> string option -> string -> unit -> t

val draw : t -> unit

val clear : t -> unit

val add_circle : t -> center:Vec.t -> radius:Float.t -> color:Vec.t -> unit
                  
