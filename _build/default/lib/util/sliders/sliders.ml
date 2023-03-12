open! Core

module Slider_dims = struct
  type t = {
      mutable button_height : Float.t;
      mutable button_width : Float.t;
      mutable center_y : Float.t;
      mutable left : Float.t;
      mutable right : Float.t;
      mutable label : string;
      mutable text_size : Float.t;
      mutable label_pos : float Array.t;
      mutable value_pos : float Array.t
    }
end
   
module Slider = struct

  type t = {
      min : Float.t;
      max : Float.t;
      mutable value : Float.t;
      mutable active : bool;
      dims : Slider_dims.t
    }

  let x_bounds t =
    let center_x =
      Helper.rescale
        ~low:t.min
        ~high:t.max
        ~new_low:t.dims.left
        ~new_high:t.dims.right
        ~value:t.value in
    let button_left =
      center_x -. t.dims.button_width /. 2. in
    let button_right =
      center_x +. t.dims.button_width /. 2. in
    (button_left, button_right)

  let y_bounds t =
    let bottom =
      t.dims.center_y -. t.dims.button_height /. 2. in
    let top =
      t.dims.center_y +. t.dims.button_height /. 2. in
    bottom, top

  let matches_xy t x y =
    let left, right = x_bounds t in
    let bottom, top = y_bounds t in
    Helper.float_within
      x ~low:left ~high:right &&
      Helper.float_within
        y ~low:bottom ~high:top

  let update_slider t x =
    let x =
      Helper.clamp x ~low:t.dims.left ~high:t.dims.right in
    let new_value =
      Helper.rescale
        ~low:t.dims.left ~high:t.dims.right
        ~new_low:t.min ~new_high:t.max
        ~value:x in
    t.value <- new_value

  let triangles t =
    let button_left, button_right =
      x_bounds t in
    let button_bottom, button_top =
      y_bounds t in
    Triangle.square_triangles
      ~left:button_left ~right:button_right
      ~top:button_top ~bottom:button_bottom


  let value t = t.value

  let update_value t new_value =
    t.value <- new_value

  let set_active t new_active =
    t.active <- new_active

  let get_text t =
    let label = t.dims.label in
    let value = Printf.sprintf "%.2G" t.value in
    ((label, t.dims.label_pos, t.dims.text_size),
     (value, t.dims.value_pos, t.dims.text_size))
      
    
    
                      

end

type t = {
    mutable num_sliders : int;
    max_sliders : int;
    sliders : Slider.t array;
    mutable selected : int option;
    mutable checked_last_click : bool
  }

let triangles t =
  Helper.f_list_to_n
    t.num_sliders
    ~f:(fun idx ->
      let slider = t.sliders.(idx) in
      let triangle_a, triangle_b =
        Slider.triangles slider in
      [triangle_a; triangle_b])
  |> List.concat

let lines t =
  Helper.f_list_to_n
    t.num_sliders
    ~f:(fun idx ->
      let slider = t.sliders.(idx) in
      let left =
        [|slider.dims.left; slider.dims.center_y; 0.|] in
      let right =
        [|slider.dims.right; slider.dims.center_y; 0.|] in
      (left, right))

let create () =
  let default_dims =
    Slider_dims.{
        button_height=0.;
        button_width=0.;
        center_y=0.;
        left=0.;
        right=0.;
        label="error";
        text_size=0.1;
        label_pos=[|0.; 0.|];
        value_pos=[|0.; 0.|]} in
  let max_sliders = 15 in
  {
    num_sliders = 0;
    max_sliders;
    sliders =
      Array.create
        ~len:max_sliders
        Slider.{
        min=0.;
        max=0.;
        value=0.;
        active=true;
        dims=default_dims
      };
    selected = None;
    checked_last_click = false
  }

let add_slider t ~min ~max ~value dims =
  let idx = t.num_sliders in
  if Int.equal t.num_sliders t.max_sliders
  then raise (Invalid_argument "max sliders")
  else
    (let slider =
       let init_value = value in
       Slider.{
           min;
           max;
           value=init_value;
           active=true;
           dims} in
     t.sliders.(idx) <- slider;
     t.num_sliders <- t.num_sliders + 1;
     slider)

let slider_matching_xy t x y =
  Array.foldi
    t.sliders
    ~init:None
    ~f:(fun idx accum slider ->
      match accum with
      | None ->
         if idx >= t.num_sliders
         then None
         else 
           (if Slider.matches_xy slider x y
            then Some idx
            else None)
      | Some idx -> Some idx)

let select_slider t click =
  match click with
  | Mouse.Click.Held (x, y) ->
     (if not t.checked_last_click
     then
       t.selected <-
         slider_matching_xy t x y);
     t.checked_last_click <- true
  | Released _ ->
     t.selected <-
       None;
     t.checked_last_click <- false

let update_selected_slider t (x, _) =
  match t.selected with
  | Some n ->
     let slider = t.sliders.(n) in
     Slider.update_slider slider x
  | None ->
     ()

let draw_and_update
      (t : t)
      (mouse : Mouse.t)
      (draw_primitives : Draw_primitives.t) =
  select_slider t mouse.click;
  update_selected_slider t mouse.pos;
  let triangles = triangles t in
  List.iteri
    triangles
    ~f:(fun idx triangle ->
      let colors =
        match t.sliders.(idx/2).active with
        | true -> Triangle.white ()
        | false -> Triangle.red () in
      Basic_triangles.add_triangle
        draw_primitives.basic_triangles
        ~positions:triangle
        ~colors);
  let lines = lines t in
  List.iter
    lines
    ~f:(fun (a, b) ->
      Lines.add_line
        draw_primitives.lines
        ~a ~b ~color:([|1.; 1.; 1.; 1.|]));
  Helper.do_n_i
    (fun idx ->
      let slider = t.sliders.(idx) in
      let ((text1, pos1, size1), (text2, pos2, size2))
        = Slider.get_text slider in
      Texts.add_string
        draw_primitives.texts text1 pos1 size1;
      Texts.add_string
        draw_primitives.texts text2 pos2 size2)
    t.num_sliders
