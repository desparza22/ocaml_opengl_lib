open Mat_vec
open! Core

type t = Draw_container.t
       
val create : unit -> t

val draw : t -> unit

val clear : t -> unit

val add_line : t -> a:Vec.t -> b:Vec.t -> color:Vec.t -> unit   
