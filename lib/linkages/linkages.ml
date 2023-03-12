open! Core
open Mat_vec

module Slider = Sliders.Slider
module Button = Buttons.Button
module Dragger = Draggers.Dragger

module Slider_dims = struct
  let button_height = 0.08
  let button_width = 0.02
  let space_between = 1.2 *. button_height

  let center_y idx =
    let idx_float = Float.of_int idx in
    1. -.
      (space_between +.
         button_height /. 2. +.
         ((space_between +. button_height) *.
            idx_float))

  let left = -0.95
  let right = -0.55
            
  let dims_of_idx idx label =
    let center_y_res = center_y idx in
    Sliders.Slider_dims.{
        button_height;
        button_width;
        center_y=center_y_res;
        left;
        right;
        label;
        text_size=button_height /. 1.5;
        label_pos=[|left; center_y_res +. button_height|];
        value_pos=[|right +. button_width *. 2.;
                    center_y_res|]}
end

    
               
(*
  states:
  
  drag and rotate
  optimizing (optionally simulating)
  drawing linkage
  adding joint


 *)
(*
module State = struct
  type t =
    | Move_links
    | Optimize of bool (*true when simulating*)
    | Add_linkage
    | Add_joint
end
 *)


               
module Link = struct
  type t =
    {
      mutable com : Vec.t;
      mutable theta : Float.t;
      mutable mobile : bool;
      mutable motor : bool
    }

  let create com theta mobile motor =
    {com; theta; mobile; motor}

  let com t = t.com

  let _set_com t value =
    t.com <- value

  let comi t idx =
    (com t).(idx)

  let set_comi t idx value =
    (com t).(idx) <- value

  let theta t = t.theta

  let set_theta t value =
    let value =
      if Helper.float_lt value 0.
      then value +. (2. *. Float.pi)
      else
        if Helper.float_gt value (2. *. Float.pi)
        then value -. (2. *. Float.pi)
        else value in
    t.theta <- value

  let rotate t incr =
    if t.motor
    then
      let cur_theta = theta t in
      set_theta t (cur_theta +. incr)
end


module Joint = struct
  module Connection = struct
    type t =
      {
        link : Link.t;
        link_coords : Vec.t
      }
      
    let position t =
      let link_x, link_y =
        Transform.rot_axes
          (Link.theta t.link) in
      Vec.Op.add_sub
        ~size:3
        [Vec.Op.Add (Link.com t.link);
         Vec.Op.Add
           (Vec.scale link_x t.link_coords.(0));
         Vec.Op.Add
           (Vec.scale link_y t.link_coords.(1))]
  end
   
  type t =
    {
      a : Connection.t;
      b : Connection.t
    }

  let positions t =
    (Connection.position t.a,
     Connection.position t.b)

  let cost t =
    let a, b = positions t in
    Vec.magnitude_sqrd (Vec.sub a b)

  let update_links t alpha_com alpha_theta =
    let update_field
          get_field set_field range alpha =
      let current = get_field () in
      let lower = current -. range /. 2. in
      let higher = current +. range /. 2. in
      let lower_cost =
        set_field lower;
        cost t in
      let higher_cost =
        set_field higher;
        cost t in
      let derivative =
        (higher_cost -. lower_cost) /. range in
      set_field (current -. derivative *. alpha)
    in
    let update_fields (link : Link.t) =
      if link.mobile
      then
        (update_field
           (fun () -> Link.comi link 0)
           (fun value -> Link.set_comi link 0 value)
           0.001
           alpha_com;
         update_field
           (fun () -> Link.comi link 1)
           (fun value -> Link.set_comi link 1 value)
           0.001
           alpha_com;
         update_field
           (fun () -> Link.theta link)
           (fun value ->
             Link.set_theta link value)
           0.01
           alpha_theta) in
    update_fields t.a.link;
    update_fields t.b.link
    
    
end

type t =
  {
    links : (Link.t * Float.t * Draggers.Dragger.t) array;
    mutable selected_link : int option;
    joints : Joint.t list;
    simulate_button : Button.t;
    mobile_button : Button.t;
    motor_button : Button.t;
    length : Slider.t;
    theta : Slider.t
  }

let cost t =
  List.fold
    t.joints
    ~init:0.
    ~f:(fun accum joint ->
      accum +. Joint.cost joint)

let rec minimize_cost
          t current_cost alpha_com alpha_theta max_rounds =
  if Int.equal max_rounds 0
  then
    ()
  else
    (List.iter
      t.joints
      ~f:(fun joint ->
        Joint.update_links joint alpha_com alpha_theta);
     let new_cost = cost t in
     if Helper.float_near_zero (current_cost -. new_cost)
     then
       ()
     else
       minimize_cost
         t new_cost alpha_com alpha_theta (max_rounds - 1))

let pos fraction length =
  length *. fraction
let back =
  pos (-0.5)
let front =
  pos 0.5
let middle _length =
  0.

let create_full
      link_info
      joint_info
      (loop_state : Main_loop.t) =
    let links_lengths_draggers =
      Array.map
        link_info
        ~f:(fun (com, theta, length, mobile, motor) ->
          let link =
            Link.create com theta mobile motor in
          let dragger =
            Draggers.add_dragger
              loop_state.draggers
              com
              ~press_f:None
              ~hold_f:None in
            (link, length, dragger)) in
    let theta =
      Sliders.add_slider
        loop_state.sliders
        ~min:0.
        ~max:(Float.pi *. 2.)
        ~value:0.
        (Slider_dims.dims_of_idx 0 "angle")
    in
    let length =
      Sliders.add_slider
        loop_state.sliders
        ~min:0.01
        ~max:2.
        ~value:1.
      (Slider_dims.dims_of_idx 1 "length")
    in
    let joint a_idx b_idx a_x b_x =
      let (link_a, length_a, _dragger_funs) =
        links_lengths_draggers.(a_idx) in
      let (link_b, length_b, _dragger_funs) =
        links_lengths_draggers.(b_idx) in
      Joint.{
          a=Connection.{
              link=link_a;
              link_coords=[|a_x length_a; 0.; 0.|]};
          b=Connection.{
              link=link_b;
              link_coords=[|b_x length_b; 0.; 0.|]}} in
    let joints =
      List.map
        joint_info
        ~f:(fun (idx_a, idx_b, pos_a, pos_b) ->
          joint idx_a idx_b pos_a pos_b) in
    let simulate_button =
      let width = 0.4 in
      let pos = ([|0.7; 0.7; 0.; 1.|]) in
      let label = "simulate" in
      let text_size =
        width /. (Float.of_int (String.length label + 1)) in
      let label_pos =
        ([|pos.(0) -.
             text_size *.
               (Float.of_int (String.length label)) /. 2.2;
           pos.(1)|]) in
      Buttons.add_button
        loop_state.buttons
        pos
        ~width ~height:0.2
        (fun state ->
          let color =
            if Int.equal state 1
            then
              ([|1.; 0.; 0.; 1.|])
            else
              ([|0.; 1.; 0.; 1.|]) in
          (color, 1 - state))
        (fun state ->
          let color =
            if Int.equal state 1
            then ([|0.; 0.7; 0.; 1.|])
            else ([|0.7; 0.; 0.; 1.|]) in
          (color, state))
        0
        ([|0.7; 0.; 0.; 1.|])
        label
        text_size
        label_pos
    in
    let mobile_button =
      let height = 0.05 in
      let label = "pinned" in
      let text_size = height in
      let label_pos = ([|-0.95; 0.55; 0.|]) in
      Buttons.add_button
        loop_state.buttons
        ([|-0.62; 0.55; 0.|])
        ~width:0.05 ~height
        (fun state ->
          let color =
            if Int.equal state 1
            then ([|0.5; 0.5; 0.5; 1.|])
            else ([|0.5; 0.7; 1.; 1.|]) in
          (color, 1 - state))
        (fun state ->
          let color =
            if Int.equal state 1
            then ([|0.5; 0.7; 1.; 1.|])
            else ([|0.5; 0.5; 0.5; 1.|]) in
          (color, state))
        0
        ([|0.5; 0.5; 0.5; 1.|])
        label
        text_size
        label_pos in
    let motor_button =
      let height = 0.05 in
      let label = "motor" in
      let text_size = height in
      let label_pos = ([|-0.95; 0.45|]) in
      Buttons.add_button
        loop_state.buttons
        ([|-0.62; 0.45; 0.|])
        ~width:0.05 ~height
        (fun state ->
          let color =
            if Int.equal state 1
            then ([|0.5; 0.5; 0.5; 1.|])
            else ([|0.5; 0.7; 1.; 1.|]) in
          (color, 1 - state))
        (fun state ->
          let color =
            if Int.equal state 1
            then ([|0.5; 0.7; 1.; 1.|])
            else ([|0.5; 0.5; 0.5; 1.|]) in
          (color, state))
        0
        ([|0.5; 0.5; 0.5; 1.|])
        label
        text_size
        label_pos in
    let t =
      {links=links_lengths_draggers;
       selected_link = None;
       joints;
       simulate_button;
       mobile_button;
       motor_button;
       theta;
       length} in
  Array.iteri
    links_lengths_draggers
    ~f:(fun idx (link, length, com_dragger) ->
      Dragger.set_press_f
        com_dragger
        (fun () ->
          t.selected_link <- Some idx;
          Slider.update_value
            t.theta link.theta;
          Slider.update_value
            t.length length;
          Button.update_state
            t.mobile_button
            (if link.mobile
             then (1, [|0.5; 0.7; 1.; 1.|])
             else (0, [|0.5; 0.5; 0.5; 1.|]));
          Button.update_state
            t.motor_button
            (if link.motor
             then (1, [|0.5; 0.7; 1.; 1.|])
             else (0, [|0.5; 0.5; 0.5; 1.|]))
        );
      Dragger.set_hold_f
        com_dragger
        (fun (x, y) ->
          link.com.(0) <- x;
          link.com.(1) <- y));
  t


let quad =
  let link_info =
    [|([|0.; 0.6; 0.|], 0., 0.4, true, true);
     ([|0.; 0.2; 0.|], 0., 0.6, true, false);
     ([|0.; -0.2; 0.|], 0., 0.8, true, false);
     ([|0.; -0.6; 0.|], 0., 0.6, true, false)|] in
  let joint_info =
    [(0, 1, front, back);
     (1, 2, front, back);
     (2, 3, front, back);
     (3, 0, front, back)] in
  create_full link_info joint_info



let four_bar =
  let link_info =
    [|([|-0.5; 0.; 0.|], 0., 0.6, false, true);(*crank*)
      ([|0.; 0.1; 0.|], 0., 1., true, false);(*coupler1*)
      ([|0.5; 0.2; 0.|], 0., 0.8, true, false);(*follower1*)
      ([|0.; 0.; 0.|], 0., 1., false, false)(*base*)|] in
  let joint_info =
    [
      (0, 3, middle, back);
      (0, 1, front, back);
      (1, 2, front, back);
      (2, 3, front, front)
    ] in
  create_full link_info joint_info

let trot_bot =
  let link_info =
    [|
      (*0. top. parallel with ground*)
      ([|0.; 0.; 0.;|], 0., 0.8, true, false);
      (*1. left. upper part*)
      ([|0.; 0.1; 0.|], 0., 0.7, true, false);
      (*2. left. lower part*)
      ([|0.; 0.2; 0.|], 0., 0.65, true, false);
      (*3. inner. connects to heel and lower left*)
      ([|0.; 0.3; 0.|], 0., 0.65, true, false);
      (*4. heel*)
      ([|0.; 0.4; 0.|], 0., 0.25, true, false);
      (*5. right. connects heel to crank*)
      ([|0.; 0.5; 0.|], 0., 0.65, true, false);
      (*6. triangle. inner*)
      ([|0.; 0.6; 0.|], 0., 0.5, true, false);
      (*7. triangle. right*)
      ([|0.; 0.7; 0.|], 0., 0.45, true, false);
      (*8. triangle. bottom*)
      ([|0.; 0.8; 0.|], 0., 0.2, true, false);
      (*9. crank*)
      ([|0.; 0.9; 0.|], 0., 0.5, false, true);
      (*10. base*)
      ([|0.; -0.1; 0.|], 0., 0.7, false, false);
      (*11. middle parallel with ground*)
      ([|0.; -0.2; 0.|], 0., 1., true, false)
    |] in
  let joint_info =
    [
      (0, 1, front, back);
      (1, 2, front, back);
      (2, 3, front, back);
      (3, 4, pos (-0.4), back);
      (4, 5, pos 0.25, back);
      (5, 9, front, front);
      (7, 9, back, front);
      (8, 9, front, front);
      (3, 8, front, back);
      (6, 8, back, back);
      (6, 7, front, front);
      (0, 6, back, front);
      (9, 10, middle, back);
      (0, 10, pos 0.3, front);
      (9, 11, front, back);
      (1, 11, pos 0.35, front)
    ] in
  create_full link_info joint_info

let multiplicator =
  let large_to_small = 3. in
  let x_to_y = Float.sqrt large_to_small in
  let y = 1.2 in
  let x = y /. x_to_y in
  let link_info =
    [|
      (*0. Oa*)
      ([|0.; 0.; 0.|], 0., x *. 2., true, false);
      (*1. EO*)
      ([|0.; 0.1; 0.|], 0., y, false, false);
      (*2. BE*)
      ([|0.; 0.2; 0.|], 0., x, true, false);
      (*3. aB*)
      ([|0.; 0.3; 0.|], 0., y, true, false);
      (*4. OS*)
      ([|0.; 0.4; 0.|], 0., y /. large_to_small, true, false);
      (*5. ST*)
      ([|0.; 0.5; 0.|], 0., x, true, false);
      (*6. OS'*)
      ([|0.; 0.6; 0.|], 0., x /. large_to_small,
       true, false);
      (*7. S'T'*)
      ([|0.; 0.7; 0.|], 0., y /. large_to_small, true, false)
    |] in
  let joint_info =
    [
      (1, 0, front, middle);
      (2, 1, front, back);
      (3, 2, front, back);
      (0, 3, front, back);
      (0, 4, middle, back);
      (4, 5, front, back);
      (5, 3, front, pos (1. /. large_to_small -. 0.5));
      (0, 6, middle, back);
      (6, 7, front, back);
      (7, 5, front, pos (1. /. large_to_small -. 0.5))
    ] in
  create_full link_info joint_info
      
let wipers =
  let link_info =
    [|
      (*0. crank*)
      ([|0.; 0.; 0.|], 0., 0.2, false, true);
      (*1. follower*)
      ([|0.; 0.1; 0.|], 0., 1., true, false);
      (*2. base*)
      ([|0.; 0.2; 0.|], 0., 0.5, false, false);
      (*3. wiper1*)
      ([|0.; 0.3; 0.|], 0., 0.7, true, false);
      (*4. wiper4*)
      ([|0.; 0.4; 0.|], 0., 0.7, true, false);
      (*5. coupler*)
      ([|0.; 0.5; 0.|], 0., 0.1, true, false)
    |] in
  let joint_info =
    [
      (0, 5, front, back);
      (5, 1, front, back);
      (1, 3, middle, back);
      (1, 4, front, back);
      (2, 3, back, pos (-0.4));
      (2, 4, front, pos (-0.4))
    ] in
  create_full link_info joint_info

let _configurations =
  [quad; four_bar; trot_bot; multiplicator; wipers]
  
let create = trot_bot

let draw_and_update t (loop_state : Main_loop.t) =
  (match t.selected_link with
  | Some idx ->
     let (link, _, com_funs) = t.links.(idx) in
     Link.set_theta link (Slider.value t.theta);
     link.mobile <-
       Int.equal (Button.state t.mobile_button) 1;
     link.motor <-
       Int.equal (Button.state t.motor_button) 1;
     let new_length = Slider.value t.length in
     t.links.(idx) <-
       (link, new_length, com_funs)
  | None -> ());
  (if Int.equal (Button.state t.simulate_button) 1
   then
     ((Array.iter
       t.links
       ~f:(fun (link, _, _) ->
         Link.rotate link (-0.04));
       minimize_cost t (cost t) 0.1 0.3 40)));
  (match t.selected_link with
   | Some idx ->
      let (link, _, _) = t.links.(idx) in
      Slider.update_value t.theta (Link.theta link)
   | None -> ());
  Array.iter
    t.links
    ~f:(fun (link, _, com_dragger) ->
      Dragger.update_pos
        com_dragger (Link.com link));
  let lines =
    Array.map
      t.links
      ~f:(fun (link, length, _) ->
        let com = Link.com link in
        let theta = Link.theta link in
        let link_x, _ = Transform.rot_axes theta in
        let a =
          Vec.sub
            com
            (Vec.scale link_x (length /. 2.)) in
        let b =
          Vec.add
            com
            (Vec.scale link_x (length /. 2.)) in
        (a, b)) in
  Array.iter
    lines
    ~f:(fun (a, b) ->
      Lines.add_line
        loop_state.draw_primitives.lines
        ~a ~b ~color:([|1.; 1.; 1.; 1.|]));
  let circles =
    List.concat_map
      t.joints
      ~f:(fun joint ->
        let pos_a, pos_b =
          Joint.positions joint in
        let cost =
          (Vec.magnitude
            (Vec.sub pos_a pos_b)) *. 8.
        in
        [(Circles.Circle.{center=pos_a;
                          radius=0.02},
          cost);
         (Circles.Circle.{center=pos_b;
                          radius=0.02},
          cost)]) in
  List.iter
    circles
    ~f:(fun (circle, cost) ->
      Filled_circles.add_circle
        loop_state.draw_primitives.filled_circles
        ~center:circle.center
        ~radius:circle.radius
        ~color:([|cost; 1. -. cost; 0.|]))  

let start () =
  Main_loop.loop_draw
    ~draw:draw_and_update
    ~state_f:create
  
