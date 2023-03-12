open! Mat_vec
open! Core

module Color_wheel = struct
  type t =
    {
      center : Vec.t;
      radius : float;
      dragger : Draggers.Dragger.t
    }

  let vertex_color index =
    let index = index mod 6 in
    let prim_strength = 0.8 in
    let comp_strength = 1. in
    let start = index / 2 in
    let color = [|0.; 0.; 0.; 1.|] in
    (match index mod 2 with
     | 0 ->
        color.(start) <- prim_strength
     | _ ->
        (color.(start) <- comp_strength;
         color.((start + 1) mod 3) <- comp_strength));
    color

  let vertex theta center radius =
    [|
      Float.cos theta *. radius +. center.(0);
      Float.sin theta *. radius +. center.(1);
      0.; 1.|]

  let dragger_color t =
    let dragger_pos = t.dragger.position in
    let incr = 6.283 /. 6. in
    let center_vert = t.center in
    let rec dragger_color_helper last_vert index =
      if Int.equal index 7
      then None
      else 
        let theta = incr *. (Float.of_int index) in
        let cur_vert = vertex theta t.center t.radius in
        let (a, b, c) =
          Triangle.barycentric_coordinates
            (Triangle.create last_vert cur_vert center_vert)
            dragger_pos in
        if Helper.float_lt a 0. ||
             Helper.float_lt b 0. ||
               Helper.float_lt c 0.
        then
          dragger_color_helper cur_vert (index + 1)
        else
          let last_color = vertex_color (index - 1) in
          let cur_color = vertex_color index in
          let center_color = Colors.white in
          Some
            (Vec.Op.add_sub
               ~size:4
               [Vec.Op.Add (Vec.scale last_color a);
                Vec.Op.Add (Vec.scale cur_color b);
                Vec.Op.Add (Vec.scale center_color c)]) in
    let last_vert = vertex 0. t.center t.radius in
    dragger_color_helper last_vert 1


end

type t = Color_wheel.t list

let dragger_radius = 0.04
       
let dragger_draw_fun
      color_wheel
      (draw_context : Draggers.Dragger.Draw_context.t)
      (draw_primitives : Draw_primitives.t) =
  let color = Color_wheel.dragger_color color_wheel in
  let color =
    match color with
    | Some color -> color
    | None -> Colors.black in
  Filled_circles.add_circle
    draw_primitives.filled_circles
    ~center:draw_context.position
    ~radius:dragger_radius
    ~color:(Colors.grayscale 0.8);
  Filled_circles.add_circle
    draw_primitives.filled_circles
    ~center:draw_context.position
    ~radius:(dragger_radius -. 0.005)
    ~color

let dragger_matches_xy
      (dragger : Draggers.Dragger.t) ~x ~y =
  Circles.Circle.intersects
    Circles.Circle.{center=dragger.position;
                    radius=dragger_radius}
  ~x ~y
       
let create (loop_state : Main_loop.t) =
  let center = ([|0.; 0.; 0.|]) in
  let dragger =
    Draggers.add_dragger
      loop_state.draggers
      center
      ~press_f:None
      ~hold_f:None in
  let color_wheel =
    Color_wheel.{center;
                 radius=0.3;
                 dragger} in
  Draggers.Dragger.set_draw_fun
    dragger (dragger_draw_fun color_wheel);
  Draggers.Dragger.set_matches_xy
    dragger dragger_matches_xy;
  [color_wheel]

let draw_and_update t (loop_state : Main_loop.t) =
  List.iter
    t
    ~f:(fun Color_wheel.{center; radius; _} ->
      Color_circles.add_circle
        loop_state.draw_primitives.color_circles
        ~center
        ~radius
        ~color:Colors.white)
    
