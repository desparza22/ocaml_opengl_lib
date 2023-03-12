open! Core

module Util : sig
  val rotation_functions : string
end

module Standard : sig
  val vertex_shader_color3 : string

  val vertex_shader_color4 : string

  val geometry_shader : string

  val fragment_shader : string
end

module Doughnut : sig

  val vertex_shader : string

  val fragment_shader : string

end

module Eight : sig

  val vertex_shader : string

  val fragment_shader : string

end

module Multi_transform : sig

  val vertex_shader : string

  val geometry_shader : string

  val fragment_shader : string

end

module Circles : sig
  val vertex_shader : string

  module Filled : sig
    val geometry_shader : string
  end

  module Outlined : sig
    val geometry_shader : string
  end

  module Color_wheel : sig
    val geometry_shader : string
  end

  val fragment_shader : string
end

module Texture : sig
  val vertex_shader : string
  val fragment_shader : string
end
     
val compile_shader : string -> int -> (int, [> `Msg of string]) Result.t


