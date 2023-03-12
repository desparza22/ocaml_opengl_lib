open Mat_vec
open! Core


module Hit_record : sig

  type t = {
      hit_point : Vec.t;
      normal : Vec.t;
      hit_dist : Float.t;
      front_face : bool
    }
         
  val create : hit_point:Vec.t -> hit_dist:Float.t -> outward_normal:Vec.t -> ray_direction:Vec.t -> t

  val hit_point : t -> Vec.t

  val normal : t -> Vec.t

end
    

module Sphere : sig
  type t =
    {center : Vec.t;
     radius : Float.t}

  val create : Vec.t -> Float.t -> t
end

module Sdf : sig
  type t =
    {
      distance_f : Vec.t -> Float.t
    }

  val create : (Vec.t -> Float.t) -> t
end

module Surface : sig

  module Material : sig
    type t =
      | Diffuse
      | Metal
      | Water
      | Light

    val roulette_probability : t -> Float.t

    val refractive_index : t -> Float.t

    val reflection_probability_density :
      t -> normal:Vec.t -> light_out:Vec.t -> light_in_flipped:Vec.t ->
      Float.t

  end
    

  type t = {
      material : Material.t;
      color : Vec.t
    }


  val create : Material.t -> Vec.t -> t

  val scatter_and_attenuation : t -> ray_in:Ray.t -> hit_record:Hit_record.t ->
                                Ray.t option

end

module Hittable : sig
  module Shape : sig    
    type t =
      Sphere of Sphere.t
    | Sdf of Sdf.t
  end

  type t =
    {
      shape : Shape.t;
      surface : Surface.t
    }


         
  val create : Shape.t -> Surface.t -> t
         
end

type t = Hittable.t list

val intersection :
  t -> Ray.t -> dist_min:Float.t -> dist_max:Float.t -> (Hit_record.t * Surface.t) option
