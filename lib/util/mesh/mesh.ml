open Mat_vec
open! Core

module Face = struct

  type t =
    {
      indices : (int * int * int);
      normal : Vec.t
    }

  let create vertices (a, b, c) =
    let vert_a = vertices.(a) in
    let vert_b = vertices.(b) in
    let vert_c = vertices.(c) in
    let a_to_b = Vec.sub vert_a vert_b in
    let a_to_c = Vec.sub vert_a vert_c in
    let normal = Vec.cross a_to_b a_to_c in
    {indices=(a, b, c); normal}
    

end


type t =
  {
    vertices : Vec.t array;
    faces : (int * int * int) array;
    
  }
