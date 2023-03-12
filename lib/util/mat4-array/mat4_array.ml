open Mat_vec
open! Core
open Result.Let_syntax

type t =
  {data : (float, Bigarray.float32_elt) Bigarray_wrapper.t;
   size : int}

let create size =
  {data = Bigarray_wrapper.create Bigarray.float32 (size * 16);
   size}

let set t ~idx ~(mat4 : Mat.t) =
  let%bind () =
    (*set an elem with a bind to propogate wrapper error msg*)
    Bigarray_wrapper.set t.data ~idx:(idx * 16 + 15) ~elem:mat4.(3).(3) in
  Ok (Helper.do_n_i
        (fun mat_idx ->
          let row = mat_idx / 4 in
          let col = mat_idx mod 4 in
          ignore
            ((Bigarray_wrapper.set
               t.data
               ~idx:(idx * 16 + mat_idx)
               ~elem:mat4.(row).(col))
            : (unit, Bigarray_wrapper.Error_type.t) result))
        16)
      
let fill_f t f =
  Helper.do_n_i
    (fun idx ->
      ignore
        ((set t ~idx ~mat4:(f idx))
        : (unit, Bigarray_wrapper.Error_type.t) result))
    t.size
  
let get t ~idx =
  let%bind _ =
    (*get an elem with a bind to propogate wrapper error msg*)
    Bigarray_wrapper.get t.data ~idx:(idx * 16) in
  Ok (Mat.init_f
    4
    (fun row col ->
      let wrapper_idx = idx + row * 4 + col in
      match Bigarray_wrapper.get t.data ~idx:wrapper_idx with
      | Ok elem -> elem
      | Error _ -> raise Exit))
