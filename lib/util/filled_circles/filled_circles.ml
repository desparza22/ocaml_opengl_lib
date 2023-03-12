open! Core

type t = Circles.t

let create =
  Circles.create
    Shaders.Circles.vertex_shader
    (Some Shaders.Circles.Filled.geometry_shader)
    Shaders.Circles.fragment_shader
  
let draw = Circles.draw
  
let clear = Circles.clear

let add_circle = Circles.add_circle
