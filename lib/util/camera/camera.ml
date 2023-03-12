open Mat_vec
open! Core

type t =
  {
    focal_length : Float.t;
    origin : Vec.t;
    horiz : Vec.t;
    vert : Vec.t;
    lower_left : Vec.t;
    width_pix : int;
    height_pix : int
  }

let create ~focal_length ~width_world ~height_world ~width_pix ~height_pix =
  let origin = [|0.; 0.; 0.|] in
  let horiz = [|width_world; 0.; 0.|] in
  let vert = [|0.; height_world; 0.|] in
  let lower_left =
    Vec.Op.add_sub
      ~size:3
      [ Vec.Op.Add origin;
        Vec.Op.Sub (Vec.scale horiz 0.5);
        Vec.Op.Sub (Vec.scale vert 0.5);
        Vec.Op.Sub [|0.; 0.; focal_length|] ] in
  {focal_length; origin; horiz; vert; lower_left; width_pix; height_pix}

let ray_to t ~x ~y ~antialias =
  let (jitter_x, jitter_y) =
    if antialias
    then (Helper.rand_between 0.0 0.9999999, Helper.rand_between 0.0 0.9999999)
    else (0., 0.) in
  let u = ((Float.of_int x) +. jitter_x) /. (Float.of_int t.width_pix) in
  let v = ((Float.of_int y) +. jitter_y) /. (Float.of_int t.height_pix) in
  let direction =
    Vec.Op.add_sub
      ~size:3
      [ Vec.Op.Add t.lower_left;
         Vec.Op.Add (Vec.scale t.horiz u);
         Vec.Op.Add (Vec.scale t.vert v);
         Vec.Op.Sub t.origin ] in
  Ray.create ~start:t.origin ~direction
