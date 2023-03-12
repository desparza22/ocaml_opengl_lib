open! Core


(* only draws triangles for now *)
type t = {
    pid : int;
    num_shapes : int;
    id : int;
    transformations_id : int;
    transformations : Mat4_array.t;
    mutable test : Float.t
  }


val create : int -> t

val draw : t  -> unit
