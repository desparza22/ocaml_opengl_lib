open Mat_vec
open! Core


type t = {
    a : Vec.t;
    b : Vec.t;
    c : Vec.t
  }

val create : Vec.t -> Vec.t -> Vec.t -> t

val create1 : Vec.t -> t

val print : t -> unit

val square_triangles : left:Float.t -> right:Float.t
  -> top:Float.t -> bottom:Float.t -> (t * t)

val barycentric_coordinates : t -> Vec.t -> (Float.t * Float.t * Float.t)

val white : unit -> t

val gray : unit -> t

val red : unit -> t                      

val green : unit -> t

val blue : unit -> t                      
