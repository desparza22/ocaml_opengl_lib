open! Core
open Mat_vec

type t =
  {
    data : Vec.t array array;
    (* todo: remove background stuff *)
    background : x:int -> y:int -> Vec.t;
    samples_per_pixel : int array array;
    mutable updated_buf1 : (int * int) list;
    mutable updated_buf2 : (int * int) list
  }


val create : width:int -> height:int -> (x:int -> y:int -> Vec.t) -> t

val width : t -> int

val height : t -> int

val add : t -> x:int -> y:int -> Vec.t -> unit

val updated : t -> bool -> (int * int) list

val clear_updated : t -> bool -> unit

val get : t -> x:int -> y:int -> Vec.t

val to_string : t -> string

val to_ppm_file : t -> string -> unit
