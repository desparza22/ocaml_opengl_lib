open! Core

type t = {
    filled_circles : Filled_circles.t;
    outlined_circles : Outlined_circles.t;
    lines : Lines.t;
    color_circles : Color_circles.t;
    basic_triangles : Basic_triangles.t;
    texts : Texts.t
  }

let _test f =
  Printf.printf "doing\n%!";
  f ()
       
let create () =
  {
    filled_circles = Filled_circles.create ();
    outlined_circles = Outlined_circles.create ();
    lines = Lines.create ();
    basic_triangles = Basic_triangles.create ();
    color_circles = Color_circles.create ();
    texts = Texts.create ()
  }

let draw_and_update t =
  Color_circles.draw t.color_circles;
  Filled_circles.draw t.filled_circles;
  Outlined_circles.draw t.outlined_circles;
  Lines.draw t.lines;
  Basic_triangles.draw t.basic_triangles;
  Texts.draw t.texts
  

let clear t =
  Color_circles.clear t.color_circles;
  Filled_circles.clear t.filled_circles;
  Outlined_circles.clear t.outlined_circles;
  Lines.clear t.lines;
  Basic_triangles.clear t.basic_triangles;
  Texts.clear t.texts
