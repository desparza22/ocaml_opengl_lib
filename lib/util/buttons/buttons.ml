open! Core
open Mat_vec 

module Button = struct
  type t = {
      mutable position : Vec.t;
      width : Float.t;
      height : Float.t;
      press_f : int -> (Vec.t * int);
      release_f : int -> (Vec.t * int);
      mutable state : int;
      mutable color_state : Vec.t;
      mutable pressed : bool;
      mutable label : string;
      mutable text_size : float;
      mutable label_pos : Vec.t
    }

  let triangles t =
    let left = t.position.(0) -. t.width /. 2. in
    let right = t.position.(0) +. t.width /. 2. in
    let top = t.position.(1) -. t.height /. 2. in
    let bottom = t.position.(1) +. t.height /. 2. in
    let (t_a, t_b) =
      Triangle.square_triangles
        ~left ~right ~top ~bottom in
    [t_a; t_b]

  let colors t =
    let color = Triangle.create1 t.color_state in
    [color; color]

  let matches_xy t ~x ~y =
    Helper.float_within
      x
      ~low:(t.position.(0) -. t.width /. 2.)
      ~high:(t.position.(0) +. t.width /. 2.)
    &&
      Helper.float_within
        y
        ~low:(t.position.(1) -. t.height /. 2.)
        ~high:(t.position.(1) +. t.height /. 2.)

  let state t = t.state

  let update_state t (state, color) =
    t.state <- state;
    t.color_state <- color
end
                   
type t = {
    mutable buttons : Button.t list;
    mutable selected : Button.t option;
    mutable checked_last_click : bool
  }

let triangles t =
  List.concat_map
    t.buttons
    ~f:Button.triangles

let colors t =
  List.concat_map
    t.buttons
    ~f:Button.colors

let create () =
  {buttons=[];
   selected=None;
   checked_last_click=false}

let add_button t position ~width ~height press_f release_f
      init_state init_color label text_size label_pos =
  let button =
    Button.{position; width; height;
            press_f; release_f;
            state=init_state;
            color_state=init_color;
            pressed=false;
            label; text_size; label_pos} in
  (t.buttons <- button::t.buttons);
  button

let button_matching_xy t x y =
  List.find
    t.buttons
    ~f:(Button.matches_xy ~x ~y)

let select_button t press_state =
  match press_state with
  | Mouse.Click.Held (x, y) ->
     if not t.checked_last_click
      then
        (match button_matching_xy t x y with
         | Some pressed ->
            let color_update, state_update =
              pressed.press_f pressed.state in
            pressed.color_state <- color_update;
            pressed.state <- state_update;
            pressed.pressed <- true;
            t.selected <- Some pressed;
            t.checked_last_click <- true           
         | None ->
            ())
     else
       ()

  | Released _ ->
     match t.selected with
     | Some button ->
        let color_update, state_update =
          button.release_f button.state in
        button.state <- state_update;
        button.color_state <- color_update;
        button.pressed <- false;
        t.selected <- None;
        t.checked_last_click <- false
     | None ->
        ()
        
          
let draw_and_update t (mouse : Mouse.t)
      (draw_primitives : Draw_primitives.t) =
  select_button t mouse.click;
  List.iter
    t.buttons
    ~f:(fun button ->
      Texts.add_string draw_primitives.texts button.label
        button.label_pos button.text_size);
  match List.iter2
          (triangles t)
          (colors t)
          ~f:(fun positions colors ->
            Basic_triangles.add_triangle
              draw_primitives.basic_triangles
              ~positions
              ~colors) with
  | Ok () -> ()
  | Unequal_lengths ->
     raise
       (Invalid_argument "shouldn't raise")
   
