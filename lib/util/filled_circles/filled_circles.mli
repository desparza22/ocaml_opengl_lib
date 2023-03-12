open! Core
open Mat_vec

type t = Circles.t


val create : unit -> t

val draw : t -> unit

val clear : t -> unit

val add_circle : t -> center:Vec.t -> radius:Float.t -> color:Vec.t -> unit
