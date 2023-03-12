open Mat_vec
open! Core
open Result.Let_syntax

type t =
  {data : (float, Bigarray.float32_elt) Bigarray_wrapper.t;
   size : int}

let create size =
  {data = Bigarray_wrapper.create Bigarray.float32 (size * 3);
   size}


let set t ~idx ~(vec3 : Vec.t) =
  let%bind () = Bigarray_wrapper.set t.data ~idx:(idx * 3) ~elem:(vec3.(0)) in
  let%bind () = Bigarray_wrapper.set t.data ~idx:(idx * 3 + 1) ~elem:(vec3.(1)) in
  Bigarray_wrapper.set t.data ~idx:(idx * 3 + 2) ~elem:(vec3.(2))

let fill_f t f =
  Helper.do_n_i
    (fun idx ->
      ignore ((set t ~idx ~vec3:(f idx))
      : (unit, Bigarray_wrapper.Error_type.t) result))
    t.size

let get t ~idx =
  let%bind x = Bigarray_wrapper.get t.data ~idx:(idx * 3) in
  let%bind y = Bigarray_wrapper.get t.data ~idx:(idx * 3 + 1) in
  let%map z = Bigarray_wrapper.get t.data ~idx:(idx * 3 + 2) in
  [| x; y; z |]
