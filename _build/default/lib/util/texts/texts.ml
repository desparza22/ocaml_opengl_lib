open Tgl3
open! Core

type t = {
    texture_id : int;
    width : int;
    height : int;
    texture_data : Vecn_array.t;
    vertex_data : Tex_triangles_basic.t
  }

let color_map =
  Array.init
    8
    ~f:(fun idx ->
      match idx < 3 with
      | true -> [|0.; 0.; 0.; 0.|]
      | false ->
         Colors.lerp_int
           ~li:3 ~hi:7 idx ~lc:Colors.white ~hc:Colors.blue)
        

let _color i =
  color_map.(i)

let color_rescale i =
  Helper.rescale ~low:0. ~high:255. ~new_low:0. ~new_high:1. ~value:(Float.of_int i)

let create () =
  let header =
    Png.check_header "glow_font3.png" in
  let width, height =
    header.header_width, header.header_height in

  let image =
    match Png.load_as_rgb24 "glow_font3.png" [] with
    | Rgb24 (image : Rgb24.t) ->
       image
    | _ ->
       raise
         (Invalid_argument "can't handle image file\n") in
  let texture_data =
    Vecn_array.create (width * height) 4 in
  Vecn_array.fill_f
    texture_data
    (fun idx ->
      let row = idx / width in
      let col = idx mod width in
      let color = Rgb24.get image col row in
      
      [|color_rescale color.r;
        color_rescale color.g;
        color_rescale color.b;
        1.|]);
  
  let store_res =
    Bigarray.Array1.create
      Bigarray.int32
      Bigarray.c_layout 1 in
  Gl.gen_textures 1 store_res;
  let texture_id = Int32.to_int_exn store_res.{0} in
  Gl.bind_texture Gl.texture_2d texture_id;
    
  Gl.tex_image2d
    Gl.texture_2d
    0
    Gl.rgba
    width
    height
    0
    Gl.rgba
    Gl.float
    (`Data texture_data.data.data);

  let vertex_data =
    Tex_triangles_basic.create () in

  {texture_id; width; height; texture_data; vertex_data}

let add_char t char position size =
  let half_size = size /. 2. in
  let tri_1, tri_2 =
    Triangle.square_triangles
      ~left:(position.(0) -. half_size)
      ~right:(position.(0) +. half_size)
      ~top:(position.(1) -. half_size)
      ~bottom:(position.(1) +. half_size) in
  let ascii = Char.to_int (Char.uppercase char) in
  let tex_width = 1. /. 10. in
  let tex_height = 1. /. 6. in
  if ascii >= 32 && ascii <= 90
  then
    (let pos = ascii - 32 in
    let row = Float.of_int (pos / 10) in
    let col = Float.of_int (pos mod 10) in
    let tex1, tex2 =
      Triangle.square_triangles
        ~left:(col *. tex_width)
        ~right:((col +. 1.) *. tex_width)
        ~top:((row +. 1.) *. tex_height)
        ~bottom:(row *. tex_height) in
    Tex_triangles_basic.add_triangle
      t.vertex_data
      ~positions:tri_1
      ~colors:(Triangle.white ())
      ~texs:tex1;
    Tex_triangles_basic.add_triangle
      t.vertex_data
      ~positions:tri_2
      ~colors:(Triangle.white ())
      ~texs:tex2)
  else
    ()

let add_string t string start_pos char_size =
  Helper.string_iteri
    string
    ~f:(fun idx char ->
      let idx = Float.of_int idx in
      let pos = [|start_pos.(0) +. idx *. char_size;
                  start_pos.(1)|] in
      add_char t char pos char_size)

let draw t =
  let red =
    Bigarray.Array1.create
      Bigarray.float32
      Bigarray.c_layout
      4 in
  red.{0} <- 1.;
  red.{1} <- 0.;
  red.{2} <- 0.;
  red.{3} <- 1.;
  Gl.tex_parameterfv
    Gl.texture_2d
    Gl.texture_border_color
    red;
  Gl.tex_parameteri
    Gl.texture_2d
    Gl.texture_min_filter
    Gl.nearest;
  Gl.tex_parameteri
    Gl.texture_2d
    Gl.texture_mag_filter
    Gl.nearest;
  Tex_triangles_basic.draw
    t.vertex_data
    
let clear t =
  Tex_triangles_basic.clear t.vertex_data

      
