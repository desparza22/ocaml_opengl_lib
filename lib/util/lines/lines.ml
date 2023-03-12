open! Core

type t = Draw_container.t

let create () =
  let fields =
    [Draw_container.Field.{name="vertex"; floats=3};
     Draw_container.Field.{name="color"; floats=4}] in
  let primitive = Draw_container.Primitive.Lines in
  let vertex_shader =
    Shaders.Standard.vertex_shader_color4 in
  let geometry_shader = None in
  let fragment_shader =
    Shaders.Standard.fragment_shader in
  Draw_container.create
    fields primitive vertex_shader geometry_shader fragment_shader ()

let draw = Draw_container.draw

let clear = Draw_container.clear

let add_line (t : t) ~a ~b ~color =
  let getter i =
    let array, shift =
      if i mod t.floats_per_vertex < 3
      then (if i < t.floats_per_vertex
            then a, 0
            else b, t.floats_per_vertex)
      else color, (if i < t.floats_per_vertex
                   then 3
                   else t.floats_per_vertex + 3) in
    array.(i - shift) in
  Draw_container.add_entry t getter
    

