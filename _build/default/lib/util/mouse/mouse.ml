open! Core

module Click = struct
  type t =
    Held of (Float.t * Float.t)
  | Released of (Float.t * Float.t)
end
             
type t =
  {
    click : Click.t;
    pos : Float.t * Float.t
  }
