open Mat_vec

type t =
  {data : (float, Bigarray.float32_elt) Bigarray_wrapper.t;
   size : int}

val create : int -> t

val set : t -> idx:int -> vec3:Vec.t -> (unit, Bigarray_wrapper.Error_type.t) Result.t

val fill_f : t -> (int -> Vec.t) -> unit

val get : t -> idx:int -> (Vec.t, Bigarray_wrapper.Error_type.t) Result.t
