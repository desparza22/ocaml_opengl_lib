open Tgl3
open! Core

let deg_to_rad deg =
  deg *. Float.pi /. 180.

let rad_to_deg rad =
  rad *. 180. /. Float.pi

let string_iteri string ~f =
  let idx = ref 0 in
  let f_adjusted char =
    (f !idx char;
     idx := !idx + 1) in
  String.iter string ~f:f_adjusted

let do_n_i f max =
  match max > 0 with
  | true ->
     let rec doer idx =
       match Int.equal idx max with
       | true ->
          ()
       | false ->
          f idx;
          doer (idx + 1) in
     doer 0
  | false ->
     ()

let fmod num denom =
  let div = num /. denom in
  let div = Float.round ~dir:(`Down) div in
  num -. (denom *. div)

    
let permutation ~low ~high =
  let arr =
    Array.init
      (high - low + 1)
      ~f:(fun idx -> idx + low) in
  do_n_i
    (fun idx ->
      let idx_swap = Random.int (Array.length arr) in
      let here = arr.(idx) in
      let there = arr.(idx_swap) in
      arr.(idx) <- there;
      arr.(idx_swap) <- here)
    (Array.length arr);
  arr

let f_list_to_n n ~f =
  match n > 0 with
  | true ->
     let rec helper idx accum =
       match Int.equal idx (-1) with
       | true -> accum
       | false -> helper (idx - 1) ((f idx)::accum) in
     helper (n - 1) []
  | false -> []


let list_to_n n =
  f_list_to_n n ~f:Fn.id
           
let rand_between low high =
  let bound = 10000 in
  let value = Random.int bound in
  let range = high -. low in
  let scaled = (Float.of_int value) /. (Float.of_int bound) *. range in
  scaled +. low

let rescale ~low ~high ~new_low ~new_high ~value =
  let percent = (value -. low) /. (high -. low) in
  let new_range = new_high -. new_low in
  new_low +. new_range *. percent

  
let clamp value ~low ~high =
  Float.min (Float.max value low) high

let lerp value ~low ~high =
  rescale ~low:0. ~high:1. ~new_low:low ~new_high:high ~value

let cos_lerp value ~low ~high =
  lerp (Float.abs (Float.cos value)) ~low ~high

let float_lt v1 v2 =
  Float.compare v1 v2 < 0

let float_lte v1 v2 =
  Float.compare v1 v2 <= 0

let float_gt v1 v2 =
  float_lt v2 v1

let float_gte v1 v2 =
  float_lte v2 v1

let float_within value ~low ~high =
  float_gte value low && float_lte value high
  
let float_near_zero value =
  let error = 0.00000000001 in
  float_within value ~low:(Float.neg error) ~high:error

let transpose arr =
  let rows = Array.length arr in
  match rows with
  | 0 -> arr
  | _ ->
     let cols = Array.length arr.(0) in
     match cols with
     | 0 -> arr
     | _ ->
        let res =
          Array.make_matrix
            ~dimx:cols ~dimy:rows arr.(0).(0) in
        for i = 0 to rows - 1 do
          for j = 0 to cols - 1 do
            res.(j).(i) <- arr.(i).(j)
          done
        done;
        res

let mapi_mat arr ~f =
  let rows = Array.length arr in
  let cols = Array.length arr.(0) in
  let res = Array.make_matrix ~dimx:rows ~dimy:cols (f 0 0 arr.(0).(0)) in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      res.(i).(j) <- f i j arr.(i).(j)
    done
  done;
  res

let map_mat arr ~f =
  mapi_mat arr ~f:(fun _ _ -> f)

let mat_init ~dimx ~dimy ~dummy_init ~f =
  let res = Array.make_matrix ~dimx ~dimy dummy_init in
  for i = 0 to dimx - 1 do
    for j = 0 to dimy - 1 do
      res.(i).(j) <- f i j
    done
  done;
  res

  
let bigarray_create kind len = Bigarray.(Array1.create kind c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int_exn a.{0}

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a; Gl.string_of_bigarray a

         
