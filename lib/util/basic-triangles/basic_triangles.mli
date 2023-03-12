open! Core

type t = Draw_container.t
  
val create : unit -> t

val draw : t -> unit

val clear : t -> unit

val add_triangle : t -> positions:Triangle.t -> colors:Triangle.t -> unit
