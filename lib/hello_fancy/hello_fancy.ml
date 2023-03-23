open! Core

type t = Draw_container.Fancy.t

let create (loop_state : Main_loop.t) =
  let fields =
    Draw_container.Shared.Field.[
        {name="vertex"; floats=3};
        {name="color"; floats=4}] in
  let vert = Shaders.Standard.vertex_shader_color4 in
  let geom_opt = None in
  let frag = Shaders.Standard.fragment_shader in
  let num_vertices = 4 in
  let vertices =
    [|
      [|0.; 1.; -1.|];
      [|-0.5; -1.; -0.7|];
       [|0.5; -1.; -0.7|];
       [|0.; -1.; -1.3|]
       |] in
  let color = Colors.purple in
  let getter vertex float_idx =
    if float_idx < 3
    then
      vertices.(vertex).(float_idx)
    else
      color.(float_idx - 3) in
  let num_triangles = 3 in
  let indices_f idx =
    let triangle = idx / 3 in
    match idx mod 3 with
    | 0 -> 0
    | res ->
       let vert_idx = triangle + res - 1 in
       (vert_idx mod 3) + 1 in
  Draw_container.Fancy.create
    fields vert geom_opt frag num_vertices getter
    num_triangles indices_f

       let draw_and_update t 
      
