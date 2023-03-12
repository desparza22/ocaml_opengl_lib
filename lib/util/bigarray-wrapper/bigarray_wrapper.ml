module Error_type = struct
  type t =
    | Index_out_of_bounds
end


type ('a, 'b) t =
  {
    data : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t;
    size : int
  }


let create kind size =
  {
    data = Bigarray.Array1.create kind Bigarray.c_layout size;
    size
  }

let fun_fill t f =
  Helper.do_n_i
    (fun idx ->
      t.data.{idx} <- f idx)
    t.size

let list_fill t l =
  List.iteri
    (fun idx elem ->
      match idx < t.size with
      | true ->
         t.data.{idx} <- elem
      | false -> ())
    l


let from_fun kind size f =
  let t = create kind size in
  fun_fill t f;
  t

let from_list kind size l =
  let t = create kind size in
  list_fill t l;
  t


let set t ~idx ~elem =
  match idx < t.size with
  | true ->
     t.data.{idx} <- elem; Ok ()
  | false ->
     Error Error_type.Index_out_of_bounds

let get t ~idx =
  match idx < t.size with
  | true -> Ok t.data.{idx}
  | false -> Error Error_type.Index_out_of_bounds
