
type ('a, 'b) t =
  {
    axis : int;
    num_axes : int;
    get_axis_val : 'a -> int -> 'b;
    subtract_axis_vals : 'b -> 'b -> Float.t;
    dist_between_elems : 'a -> 'a -> Float.t;
    lower : (('a, 'b) t * 'b) option;
    higher : (('a, 'b) t * 'b) option;
    data : 'a option
  }
    
val create : num_axes:int ->
             get_axis_val:('a -> int -> 'b) ->
             subtract_axis_vals:('b -> 'b -> Float.t) ->
             dist_between_elems:('a -> 'a -> Float.t) ->
             data:('a Array.t) ->
             ('a, 'b) t

val nearest_k :
  ('a, 'b) t ->
  'a ->
  int ->
  'a Sorted.t
