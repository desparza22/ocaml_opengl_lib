open! Mat_vec
open! Core


type t = {
    left_point : Vec.t;
    right_point : Vec.t;
    crank_length : unit -> Float.t;
    coupler_length : unit -> Float.t;
    follower_length : unit -> Float.t;
    mutable crank_theta : Float.t;
    mutable last_trail_point : Vec.t option;
    trail_lines : (Vec.t * Vec.t) Doubly_linked.t;
    mutable trail_length : int;
    mutable last_trail_point' : Vec.t option;
    trail_lines' : (Vec.t * Vec.t) Doubly_linked.t;
    mutable trail_length' : int
  }
       
val start : unit -> unit
