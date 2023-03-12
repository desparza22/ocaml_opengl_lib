open Mat_vec
open! Core

type t =
  {
    start : Vec.t;
    norm_direction : Vec.t
  }

val create : start:Vec.t -> direction:Vec.t -> t


val point_at : t -> Float.t -> Vec.t

