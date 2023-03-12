open! Mat_vec
open! Core

type t = Draw_container.t

let create () =
  let fields =
    [Draw_container.Field.{name="vertex"; floats=3};
     Draw_container.Field.{name="color"; floats=4}] in
  let primitive = Draw_container.Primitive.Triangles in
  let vertex_shader =
    Shaders.Standard.vertex_shader_color4 in
  let geometry_shader = None in
  let fragment_shader =
    Shaders.Standard.fragment_shader in
  Draw_container.create
    fields primitive vertex_shader geometry_shader fragment_shader ()

let draw = Draw_container.draw

let clear = Draw_container.clear

let add_triangle (t : t)
      ~(positions : Triangle.t) ~(colors : Triangle.t) =
  let getter i =
    let triangle, shift =
      if i mod t.floats_per_vertex < 3
      then positions, 0
      else colors, 3 in
    let triangle_entry =
      match i / t.floats_per_vertex with
      | 0 -> triangle.a
      | 1 -> triangle.b
      | 2 -> triangle.c
      | _ -> raise
               (Invalid_argument
                  "bad value passed to getter, basic_triangles.add_triangle\n")
    in
    let triangle_i =
      (i mod t.floats_per_vertex) - shift in
    triangle_entry.(triangle_i) in
  Draw_container.add_entry t getter
      
             
