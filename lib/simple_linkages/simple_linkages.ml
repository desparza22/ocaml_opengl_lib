open! Mat_vec
open! Core

type t = {
    left_point : Vec.t;
    right_point : Vec.t;
    crank_length : unit -> Float.t;
    coupler_length : unit -> Float.t;
    follower_length : unit -> Float.t;
    mutable crank_theta : Float.t;
    mutable last_trail_point : Vec.t option;
    trail_lines : (Vec.t * Vec.t) Doubly_linked.t;
    mutable trail_length : int;
    mutable last_trail_point' : Vec.t option;
    trail_lines' : (Vec.t * Vec.t) Doubly_linked.t;
    mutable trail_length' : int
  }

(* can optimize by passing distance_sqrd and comparing to 
   distance_mid_sqrd *)
let rec points_along_at_dist_helper
          circle point distance
          theta_min theta_max inverse_search =
  match Helper.float_near_zero (theta_max -. theta_min) with
  | true -> Circles.Circle.point_along circle theta_max
  | false ->
     let theta_mid = (theta_min +. theta_max) /. 2. in
     let point_mid =
       Circles.Circle.point_along circle theta_mid in
     let distance_mid =
       Vec.magnitude (Vec.sub point point_mid) in
     let above_mid =
       match Helper.float_gt distance distance_mid with
       | true -> not inverse_search
       | false -> inverse_search in
     if above_mid
     then
       points_along_at_dist_helper
         circle point distance
         theta_mid theta_max inverse_search
     else
       points_along_at_dist_helper
         circle point distance
         theta_min theta_mid inverse_search
     
(* can optimize by finding one point and using symmetry to 
   calculate second point *)
let points_along_at_dist
      (circle : Circles.Circle.t) point distance =
  let center_dist =
    Vec.magnitude (Vec.sub circle.center point) in
  if Helper.float_within
       distance
       ~low:(center_dist -. circle.radius)
       ~high:(center_dist +. circle.radius)
  then
    let near_theta =
      Circles.Circle.theta_of_point circle point in
    let far_theta = 
      near_theta +. Float.pi in
    let point1 =
      points_along_at_dist_helper
        circle point distance near_theta far_theta false in
    let point2 =
      points_along_at_dist_helper
        circle point distance
        far_theta (far_theta +. Float.pi) true in
    Some (point1, point2)
  else
    None

let trail_length = 150

let lines_and_circles t =
  let crank_circle =
    Circles.Circle.{center=t.left_point;
                    radius=t.crank_length ()} in
  let crank_coupler_joint =
    Circles.Circle.point_along crank_circle t.crank_theta in
  let follower_circle =
    Circles.Circle.{center=t.right_point;
                    radius=t.follower_length ()} in
  let coupler_follower_joints =
    points_along_at_dist
      follower_circle
      crank_coupler_joint
      (t.coupler_length ()) in
  let lines =
    match coupler_follower_joints with
    | Some (point_a, point_b) ->
       let next_trail_point =
         Vec.add
           (Vec.scale crank_coupler_joint 0.7)
           (Vec.scale point_a 0.3) in
       (match t.last_trail_point with
       | Some last_trail_point ->
          ignore
            (Doubly_linked.insert_first
               t.trail_lines
               (last_trail_point, next_trail_point));
          t.trail_length <- t.trail_length + 1
       | None -> ());
       t.last_trail_point <- Some next_trail_point;
       let next_trail_point' =
         Vec.add
           (Vec.scale crank_coupler_joint 0.3)
           (Vec.scale point_b 0.7) in
       (match t.last_trail_point' with
       | Some last_trail_point' ->
          ignore
            (Doubly_linked.insert_first
               t.trail_lines'
               (last_trail_point', next_trail_point'));
          t.trail_length' <- t.trail_length' + 1
       | None -> ());
       t.last_trail_point' <- Some next_trail_point';
       [(t.left_point, crank_coupler_joint);
        (crank_coupler_joint, point_a);
        (crank_coupler_joint, point_b);
        (point_a, t.right_point);
        (point_b, t.right_point)]
    | None ->
       t.last_trail_point <- None;
       t.last_trail_point' <- None;
       [(t.left_point, crank_coupler_joint)] in
  (if t.trail_length > trail_length
  then
    (ignore
       (Doubly_linked.remove_last t.trail_lines);
     t.trail_length <- t.trail_length - 1)
  else
    ());
  let trail_lines =
    Doubly_linked.fold_right
      ~init:([])
      ~f:(fun elem list -> elem::list)
      t.trail_lines in
  let trail_lines =
    List.rev trail_lines in
  let trail_lines =
    Lines.create
      ~colors_f:(fun idx ->
        let strength =
          Float.of_int (trail_length - idx) in
        let strength =
          strength /. (Float.of_int trail_length) in
        [|1. -. strength; strength; strength; 1.|])
      trail_lines in
  (if t.trail_length' > trail_length
  then
    (ignore
       (Doubly_linked.remove_last t.trail_lines');
     t.trail_length' <- t.trail_length' - 1)
  else
    ());
  let trail_lines' =
    Doubly_linked.fold_right
      ~init:([])
      ~f:(fun elem list -> elem::list)
      t.trail_lines' in
  let trail_lines' =
    List.rev trail_lines' in
  let trail_lines' =
    Lines.create
      ~colors_f:(fun idx ->
        let strength =
          Float.of_int (trail_length - idx) in
        let strength =
          strength /. (Float.of_int trail_length) in
        [|1. -. strength; strength; strength; 1.|])
      trail_lines' in
  let circles =
    [] in
  (*[crank_circle; follower_circle] in*)
  (trail_lines, trail_lines',
   Lines.create lines, Circles.create circles)

let state_create (loop_state : Main_loop.t) =
  let left_point = ([|-0.45; 0.; 0.|]) in
  let crank_theta = Float.pi in
  let crank_length = 0.2 in
  let coupler_length = 0.8 in
  let follower_length = 0.4 in
  let right_point = ([|0.3; 0.; 0.|]) in
  let crank_length =
    Sliders.add_slider
      loop_state.sliders 0. 1.5 crank_length in
  let coupler_length =
    Sliders.add_slider
      loop_state.sliders 0. 1.5 coupler_length in
  let follower_length =
    Sliders.add_slider
      loop_state.sliders 0. 1.5 follower_length in
  {left_point; right_point;
   crank_length; coupler_length; follower_length;
   crank_theta;
   last_trail_point=None;
   trail_lines=Doubly_linked.create ();
   trail_length=0;
   last_trail_point'=None;
   trail_lines'=Doubly_linked.create ();
   trail_length'=0}

let draw_and_update
      (t : t) (_loop_state : Main_loop.t) =
  t.crank_theta <- t.crank_theta +. 0.03;
  let trail_lines, trail_lines', lines, circles =
    lines_and_circles t in
  Lines.draw trail_lines;
  Lines.draw trail_lines';
  Lines.draw lines;
  Circles.draw circles

let start () =
  Main_loop.loop_draw
    ~draw:draw_and_update
    ~state_f:state_create
           
