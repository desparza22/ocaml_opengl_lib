open! Core

type t = {
    filled_circles : Filled_circles.t;
    outlined_circles : Outlined_circles.t;
    lines : Lines.t;
    color_circles : Color_circles.t;
    basic_triangles : Basic_triangles.t;
    texts : Texts.t
  }

val create : unit -> t
       
val draw_and_update : t -> unit

val clear : t -> unit
