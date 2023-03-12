open! Core

module Vec = struct


  type t = Float.t array    
         
  let size t = Array.length t

  let map2 f t1 t2 =
    Array.mapi
      ~f:(fun idx t1_elem -> f t1_elem t2.(idx))
      t1

  let dot t1 t2 =
    Array.fold_right
      (map2 ( *. ) t1 t2)
      ~f:(+.)
      ~init:0.

  let cross t1 t2 =
    [|
      t1.(1) *. t2.(2) -. t1.(2) *. t1.(1);
      t1.(2) *. t2.(0) -. t1.(0) *. t2.(2);
      t1.(0) *. t2.(1) -. t1.(1) *. t2.(1)
    |]

  let mult t1 t2 =
    Array.mapi
      t1
      ~f:(fun idx elem1 -> let elem2 = t2.(idx) in elem1 *. elem2)

  let neg t =
    Array.map
      ~f:Float.neg
      t

  let add t1 t2 =
    map2 (+.) t1 t2

  let sub t1 t2 =
    add t1 (neg t2)

  let scale t scalar =
    Array.map ~f:(fun x -> x *. scalar) t

  let magnitude_sqrd t =
    dot t t
    
  let magnitude t =
    Float.sqrt (magnitude_sqrd t)

  let normalize t =
    let mag = magnitude t in
    match Float.equal Float.zero mag with
    | true -> Error "normalizing zero vector"
    | false -> Ok (scale t (1. /. mag))

  let normalize_exn t =
    match normalize t with
    | Ok normalized -> normalized
    | Error msg ->
       Printf.printf "normalize_exn raised";
       raise (Invalid_argument msg)

  let average t1 t2 =
    scale
      (add t1 t2)
      0.5

  let to_string t =
    Array.fold_right
      t
      ~f:(fun elem build -> build ^ (Printf.sprintf "%f " elem))
      ~init:""

  let homogeneous_point t =
    [| t.(0); t.(1); t.(2); 1.0 |]

  let homogeneous_vector t =
    [| t.(0); t.(1); t.(2); 0.0 |]

  let of_homogeneous t =
    [| t.(0); t.(1); t.(2) |]


  let rec random_within_unit () =
    let random_vec =
      [| Helper.rand_between (-1.) 1.; Helper.rand_between (-1.) 1.; Helper.rand_between 0. 1. |] in
    match Helper.float_lte (magnitude_sqrd random_vec) 1. with
    | true -> random_vec
    | false -> random_within_unit ()
    
  module Op = struct
    type nonrec t =
      Add of t
    | Sub of t

    let rec add_sub_helper ops accum =
      match ops with
        [] -> accum
      | h::t ->
         let new_accum = 
           match h with
             Add other -> add accum other
           | Sub other -> sub accum other in
         add_sub_helper t new_accum

    let add_sub ~size ops =
      add_sub_helper ops (Array.create ~len:size 0.)

  end

end


module Mat = struct
  
  type t = Float.t array array
         
  let init_f dim f =
    Array.init
      dim
      ~f:(fun i ->
        Array.init
          dim
          ~f:(fun j -> f i j))

  let mult_m t1 t2 =
    let dim = Array.length t1 in
    init_f
      dim
      (fun i j ->
        let res = ref 0.0 in
        for k = 0 to dim - 1 do
          res := !res +. t1.(i).(k) *. t2.(k).(j)
        done;
        !res)

  let mult_v t v =
    Array.init
      (Array.length v)
      ~f:(fun i ->
        let multiplied = Vec.mult t.(i) v in
        Array.fold_right
          multiplied
          ~f:(+.)
          ~init:0.)

  let identity () =
    init_f 4 (fun row col -> if Int.equal row col then 1. else 0.)
end

module Transform = struct

  module Axis = struct
    type t = X | Y | Z

  end

  let rotationX theta =
    let cos_t = Float.cos theta in
    let sin_t = Float.sin theta in
    [|
      [| 1.0; 0.0; 0.0; 0.0 |];
      [| 0.0; cos_t; Float.neg sin_t; 0.0 |];
      [| 0.0; sin_t; cos_t; 0.0 |];
      [| 0.0; 0.0; 0.0; 1.0 |] |]

  let rotationY theta =
    let cos_t = Float.cos theta in
    let sin_t = Float.sin theta in
    [|
      [| cos_t; 0.0; sin_t; 0.0 |];
      [| 0.0; 1.0; 0.0; 0.0 |];
      [| Float.neg sin_t; 0.0; cos_t; 0.0 |];
      [| 0.0; 0.0; 0.0; 1.0 |] |]

  let rotationZ theta =
    let cos_t = Float.cos theta in
    let sin_t = Float.sin theta in
    [|
      [| cos_t; Float.neg sin_t; 0.0; 0.0 |];
      [| sin_t; cos_t; 0.0; 0.0 |];
      [| 0.0; 0.0; 1.0; 0.0 |];
      [| 0.0; 0.0; 0.0; 1.0 |] |]      

  let rotation axis =
    match axis with
    | Axis.X -> rotationX
    | Y -> rotationY
    | Z -> rotationZ


  let translation (delta : Vec.t) =
    [|
      [| 1.0; 0.0; 0.0; delta.(0) |];
      [| 0.0; 1.0; 0.0; delta.(1) |];
      [| 0.0; 0.0; 1.0; delta.(2) |];
      [| 0.0; 0.0; 0.0; 1.0 |] |]

  (* untested *)
  let scaling (s : Vec.t) =
    [|
      [| s.(0); 0.0; 0.0; 0.0 |];
      [| 0.0; s.(1); 0.0; 0.0 |];
      [| 0.0; 0.0; s.(2); 0.0 |];
      [| 0.0; 0.0; 0.0; 0.0 |] |]

  let rot_axes theta =
    let x = [|1.; 0.; 0.; 1.|] in
    let y = [|0.; 1.; 0.; 1.|] in
    let rot = rotationZ theta in
    (Mat.mult_v rot x,
     Mat.mult_v rot y)

  module Op = struct
    type nonrec t =
      | Rotate of Axis.t * Float.t
      | Translate of Vec.t
      | Scale of Vec.t

    let rec transform_helper ops accum =
      match ops with
      | op::rest ->
         let transformation =
           match op with
           | Rotate (axis, theta) -> rotation axis theta
           | Translate t_vec -> translation t_vec
           | Scale s_vec -> scaling s_vec in
         transform_helper rest (Mat.mult_m transformation accum)
      | [] -> accum

    let local_identity = Mat.identity ()

    let transform ops =
      transform_helper ops local_identity
  end
end

                     
