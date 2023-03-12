
type t =
  {data : Vec3_array.t;
   size : int}

val create : int -> t

val set : t -> idx:int -> triangle:Triangle.t -> (unit, Bigarray_wrapper.Error_type.t) Result.t

val fill_f : t -> (int -> Triangle.t) -> unit

val get : t -> idx:int -> (Triangle.t, Bigarray_wrapper.Error_type.t) Result.t

val print : t -> unit

                                           
