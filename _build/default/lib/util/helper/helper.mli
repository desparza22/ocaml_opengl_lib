open Core

val permutation : low:int -> high:int -> int array

val deg_to_rad : Float.t -> Float.t

val fmod : Float.t -> Float.t -> Float.t

val rad_to_deg : Float.t -> Float.t

val string_iteri : String.t -> f:(int -> char -> unit) -> unit

val do_n_i : (int -> unit) -> int -> unit

val list_to_n : int -> int list

val f_list_to_n : int -> f:(int -> 'a) -> 'a list

val rand_between : Float.t -> Float.t -> Float.t

val rescale : low:Float.t ->
              high:Float.t ->
              new_low:Float.t ->
              new_high:Float.t ->
              value:Float.t ->
              Float.t

val clamp : Float.t -> low:Float.t -> high:Float.t -> Float.t

val lerp : Float.t -> low:Float.t -> high:Float.t -> Float.t

val cos_lerp : Float.t -> low:Float.t -> high:Float.t -> Float.t

val float_lt : Float.t -> Float.t -> bool

val float_lte : Float.t -> Float.t -> bool

val float_gt : Float.t -> Float.t -> bool

val float_gte : Float.t -> Float.t -> bool

val float_within : Float.t -> low:Float.t -> high:Float.t -> bool

val float_near_zero : Float.t -> bool

val transpose : 'a array array -> 'a array array

val mapi_mat : 'a array array -> f:(int -> int -> 'a -> 'b) -> 'b array array

val map_mat : 'a array array -> f:('a -> 'b) -> 'b array array

val mat_init : dimx:int -> dimy:int -> dummy_init:'a -> f:(int -> int -> 'a) -> 'a array array

val bigarray_create : ('a, 'b) Bigarray.kind -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

val get_int : ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) -> int

val get_string : int -> ((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> unit) -> string

                                                                                                               
