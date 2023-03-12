open! Core
open Mat_vec

let button_radius = 0.02

module Dragger = struct
  module Draw_context = struct
    type t = {
        time : Float.t;
        position : Vec.t;
        active : bool
      }

    let circle t =
      let color =
        if t.active
        then [|1.; 1.; 1.|]
        else [|0.6; 0.6; 0.6|] in
      Some
        (Circles.Circle.{center=t.position;
                         radius=button_radius},
         color)
  end

  type t = {
      mutable position : Vec.t;
      mutable active : bool;
      mutable press_f : unit -> unit;
      mutable hold_f : (Float.t * Float.t) -> unit;
      mutable draw_fun :
                Draw_context.t -> Draw_primitives.t -> unit;
      mutable matches_xy : t -> x:float -> y:float -> bool
    }

  let default_draw_fun
        (draw_context : Draw_context.t)
        (draw_primitives : Draw_primitives.t) =
    let expand_strength =
      (((Float.cos draw_context.time +. 1.)
        /. 2.) +. 3.) /. 4. in
    let circle = Draw_context.circle draw_context in
    match circle with
    | Some (circle, color) ->
       Filled_circles.add_circle
         draw_primitives.filled_circles
         ~center:circle.center
         ~radius:(circle.radius *. expand_strength)
         ~color
    | None -> ()

  let default_matches_xy (t : t) ~x ~y =
    t.active &&
      Circles.Circle.intersects
        Circles.Circle.{center=t.position;
                        radius=button_radius}
        ~x ~y
      
  let get_pos t =
    t.position

  let update_pos t pos =
    t.position <- pos

  let set_active t active =
    t.active <- active

  let set_press_f t press_f =
    t.press_f <- press_f

  let set_hold_f t hold_f =
    t.hold_f <- hold_f

  let set_draw_fun t draw_fun =
    t.draw_fun <- draw_fun

  let set_matches_xy t matches_xy =
    t.matches_xy <- matches_xy
end      

type t = {
    mutable draggers : Dragger.t list;
    mutable selected : Dragger.t option;
    mutable checked_last_click : bool;
    mutable time : Float.t
  }

let create () =
  {draggers=[];
   selected=None;
   checked_last_click=false;
   time=0.}

let add_dragger t position ~press_f ~hold_f =
  if not (Int.equal (Array.length position) 3)
  then
    raise
      (Invalid_argument
         "dragger position should have 3 coordinates")
  else
    let press_f =
      match press_f with
      | Some f -> f
      | None -> fun () -> () in
    let hold_f =
      match hold_f with
      | Some f -> f
      | None -> fun (_ : Float.t * Float.t) -> () in
    let dragger =
      Dragger.{
          position;
          active=true;
          press_f;
          hold_f;
          draw_fun=Dragger.default_draw_fun;
          matches_xy=Dragger.default_matches_xy} in
    (t.draggers <- dragger::t.draggers);
    dragger

let dragger_matching_xy t x y =
  List.find
    t.draggers
    ~f:(fun dragger ->
      dragger.matches_xy dragger ~x ~y)

let select_dragger t (mouse : Mouse.t) =
  match mouse.click with
  | Mouse.Click.Held (x, y) ->
     (if not t.checked_last_click
     then
       t.selected <-
         dragger_matching_xy t x y);
     (match t.selected with
      | Some dragger ->
         let (x, y) = mouse.pos in
         dragger.position <- [|x; y; 0.|];
         (if not t.checked_last_click
          then dragger.press_f ());
         dragger.hold_f (x, y)
     | None -> ());
     t.checked_last_click <- true
     
  | Released _ ->
     t.selected <-
       None;
     t.checked_last_click <- false

let draw_and_update
      (t : t) (mouse : Mouse.t)
      (draw_primitives : Draw_primitives.t) =
  select_dragger t mouse;
  List.iter
    t.draggers
    ~f:(fun dragger ->
      let draw_context =
        Dragger.Draw_context.{
            time=t.time;
            position=dragger.position;
            active=dragger.active} in
      dragger.draw_fun
        draw_context draw_primitives);
  t.time <- t.time +. 0.025
  
