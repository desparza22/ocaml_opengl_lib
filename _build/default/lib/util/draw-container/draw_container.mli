open! Core


module Shared : sig
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
end

module Basic : sig
  type t

  val create : Shared.Field.t list -> Shared.Primitive.t ->
               string -> string option -> string -> t

  val draw : t -> unit

  val clear : t -> unit

  val add_entry : t -> (int -> float) -> unit

  val floats_per_vertex : t -> int
end

module Fancy : sig
  type t

  val create : fields:Shared.Field.t list ->
               vert:string ->
               geom_opt:string option ->
               frag:string ->
               num_vertices:int ->
               getter:(int -> int -> float) ->
               num_triangles:int ->
               indices_f:(int -> int) -> t

  val draw : t -> unit
end
