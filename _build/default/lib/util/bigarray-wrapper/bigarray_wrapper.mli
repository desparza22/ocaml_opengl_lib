module Error_type : sig
  type t =
    | Index_out_of_bounds
end

type ('a, 'b) t =
  {data : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t;
   size : int }


val create : ('a, 'b) Bigarray.kind -> int -> ('a, 'b) t


val from_fun : ('a, 'b) Bigarray.kind -> int -> (int -> 'a) -> ('a, 'b) t

val from_list : ('a, 'b) Bigarray.kind -> int -> 'a list -> ('a, 'b) t


val fun_fill : ('a, 'b) t -> (int -> 'a) -> unit

val list_fill : ('a, 'b) t -> 'a list -> unit

val set : ('a, 'b) t -> idx:int -> elem:'a -> (unit, Error_type.t) Result.t

val get : ('a, 'b) t -> idx:int -> ('a, Error_type.t) result
