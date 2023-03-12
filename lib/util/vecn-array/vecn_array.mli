open Mat_vec

type t =
  {
    n : int;
    data : (float, Bigarray.float32_elt) Bigarray_wrapper.t;
    size : int}

val create : int -> int -> t

val set : t -> idx:int -> vecn:Vec.t -> unit

val fill_f : t -> (int -> Vec.t) -> unit

val get : t -> idx:int -> Vec.t
