open! Core

module Field : sig
  type t = {
      name : string;
      floats : int
    }
end

module Primitive : sig
  type t =
    Points
  | Lines
  | Triangles
end

                     
type t = {
    pid : int;
    mutable num_primitives : int;
    id : int;
    data_id : int;
    mutable data : (float, Bigarray.float32_elt) Bigarray_wrapper.t;
    mutable data_capacity : int;
    fields : Field.t list;
    primitive : Primitive.t;
    floats_per_vertex : int;
    vertices_per_entry : int;
    floats_per_entry : int
  }

val create : Field.t list -> Primitive.t -> string -> string option ->
             string -> unit -> t

val draw : t -> unit

val clear : t -> unit

val add_entry : t -> (int -> float) -> unit
