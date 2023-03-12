open! Core

let lerp_float ~lf ~hf value ~lc ~hc =
  Array.mapi
    ~f:(fun idx arr1 ->
      let arr2 = hc.(idx) in
      Helper.rescale ~low:lf ~high:hf
        ~new_low:arr1 ~new_high:arr2 ~value)
    lc

let lerp_int ~li ~hi value ~lc ~hc =
  lerp_float ~lf:(Float.of_int li) ~hf:(Float.of_int hi)
    (Float.of_int value) ~lc ~hc
  


let white =
  [|1.; 1.; 1.; 1.|]

let red =
  [|249./.255.;
    38./.255.;
    114./.255.;
    1.|]

let orange =
  [|253./.255.;
    151./.255.;
    31./.255.;
    1.|]

let yellow =
  [|1.; 1.; 0.; 1.|]

let green =
  [|166./.255.;
    226./.255.;
    46./.255.;
    1.|]

let blue =
  [|102./.255.;
    217./.255.;
    239./.255.;
    1.|]

let light_blue =
  [|166./.255.;
    189./.255.;
    215./.255.;
    1.|]

let purple =
  [|174./.255.;
    129./.255.;
    255./.255.;
    1.|]

let brown =
  [|123./.255.;
    63./.255.;
    0.;
    1.|]

let gray =
  [|0.5; 0.5; 0.5; 1.|]

let black =
  [|0.; 0.; 0.; 1.|]

let grayscale scale =
  [|scale; scale; scale; 1.|]

let color_index =
  [|red; orange; yellow; green;
    blue; light_blue; purple; brown|]

let num_colors =
  Array.length color_index

let colori i =
  color_index.(i mod num_colors)
