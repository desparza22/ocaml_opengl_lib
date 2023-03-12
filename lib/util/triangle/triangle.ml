open Mat_vec
open! Core
   
type t = {
    a : Vec.t;
    b : Vec.t;
    c : Vec.t
  }

let create a b c =
  {a; b; c}

let create1 a =
  {a; b=a; c=a}

let print t =
  Printf.printf
    "Tri: %s\n%s\n%s\n"
    (Vec.to_string t.a)
    (Vec.to_string t.b)
    (Vec.to_string t.c)

let square_triangles ~left ~right ~top ~bottom =
  let low_left =
      [|left; bottom; 0.|] in
  let low_right =
    [|right; bottom; 0.|] in
  let high_left =
    [|left; top; 0.|] in
  let high_right =
    [|right; top; 0.|] in
  (create
     low_left high_left high_right,
   create
     low_left low_right high_right)

let barycentric_coordinates t point =
  let sub' a b =
    [|a.(0) -. b.(0); a.(1) -. b.(1); a.(2) -. b.(2)|] in
  let v0 = sub' t.b t.a in
  let v1 = sub' t.c t.a in
  let v2 = sub' point t.a in
  let d00 = Vec.dot v0 v0 in
  let d01 = Vec.dot v0 v1 in
  let d11 = Vec.dot v1 v1 in
  let d20 = Vec.dot v2 v0 in
  let d21 = Vec.dot v2 v1 in
  let denom = d00 *. d11 -. d01 *. d01 in
  let b = (d11 *. d20 -. d01 *. d21) /. denom in
  let c = (d00 *. d21 -. d01 *. d20) /. denom in
  let a = 1. -. b -. c in
  (a, b, c)

let white () =
  create1 [|1.; 1.; 1.; 1.|]

let gray () =
  create1 [|0.6; 0.6; 0.6; 1.|]

let red () =
  create1 [|1.; 0.; 0.; 1.|]
    
let green () =
  create1 [|0.; 1.; 0.; 1.|]

let blue () =
  create1 [|0.; 0.; 1.; 1.|]
