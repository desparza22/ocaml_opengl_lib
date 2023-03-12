open Mat_vec
open! Core

type t =
  {
    start : Vec.t;
    norm_direction : Vec.t
  }

let create ~start ~direction =
  let norm_direction = Vec.normalize_exn direction in
  {start; norm_direction}

let point_at t dist =
  Vec.add t.start (Vec.scale t.norm_direction dist)
