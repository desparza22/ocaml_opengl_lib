open Mat_vec
open! Core

type t = {
    center : Vec.t;
    shapes : Shape.t list;
    transformations : Mat.t list
  }
