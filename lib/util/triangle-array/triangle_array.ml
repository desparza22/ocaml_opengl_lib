open! Core
open Result.Let_syntax

type t =
  {data : Vec3_array.t;
   size : int}

let create size =
  {data = Vec3_array.create (size * 3);
   size}

let set t ~idx ~(triangle : Triangle.t) =
  let%bind () = Vec3_array.set t.data ~idx:(idx * 3) ~vec3:(triangle.a) in
  let%bind () = Vec3_array.set t.data ~idx:(idx * 3 + 1) ~vec3:(triangle.b) in
  Vec3_array.set t.data ~idx:(idx * 3 + 2) ~vec3:(triangle.c)

let fill_f t f =
  Helper.do_n_i
    (fun idx ->
      ignore
        ((set t ~idx ~triangle:(f idx))
        : (unit, Bigarray_wrapper.Error_type.t) result))
    t.size
  
let get t ~idx =
  let%bind a = Vec3_array.get t.data ~idx:(idx * 3) in
  let%bind b = Vec3_array.get t.data ~idx:(idx * 3 + 1) in
  let%map c = Vec3_array.get t.data ~idx:(idx * 3 + 2) in
  Triangle.create a b c


let print t =
  Helper.do_n_i
    (fun idx ->
      ignore
        ((let%map tri = get t ~idx in
          Triangle.print tri)
        : (unit, Bigarray_wrapper.Error_type.t) result))
    t.size
