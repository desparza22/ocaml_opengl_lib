type 'a t =
  {data : 'a Array.t;
   mutable num_elems : int;
   comparer : 'a -> 'a -> int}

val create :
  size:int -> witness:'a -> comparer:('a -> 'a -> int) -> 'a t

val insert :
  'a t -> 'a -> unit

val get :
  'a t -> int -> 'a option
