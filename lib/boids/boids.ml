open Tgl3
open Mat_vec
open! Core
open Result.Let_syntax

module Boid = struct

  module Axis = struct
    type t = X | Y | Z
  end

  type t =
    { mutable center : Vec.t;
      mutable homogeneous_center : Vec.t;
      mutable norm_direction : Vec.t;
      mutable homogeneous_norm_direction : Vec.t;
      mutable velocity : Float.t
    }


  let normalize_direction direction velocity =
    match Vec.normalize direction with
    | Ok norm_direction ->
       norm_direction, velocity
    | Error _ ->
       ([| 1.0; 0.0; 0.0|], 0.0)


  let create center direction velocity =
    let homogeneous_center =
      Vec.homogeneous_point center in
    let norm_direction, velocity = 
      normalize_direction direction velocity in
    let homogeneous_norm_direction =
      Vec.homogeneous_vector norm_direction in
    {center;
     homogeneous_center;
     norm_direction; 
     homogeneous_norm_direction;
     velocity}

  let right_tail_rotate = 6.28 /. 5. *. 2.
  let left_tail_rotate = 6.28 /. 5. *. 3.

  let get_triangle t =
    let a = Vec.add
              t.center
              (Vec.scale t.norm_direction 0.004) in
    let calc_tail_from_rot rot =
      t.homogeneous_norm_direction |>
        (Mat.mult_v
           (Transform.rotation Transform.Axis.Z rot)) |>
        Vec.of_homogeneous |>
        Vec.normalize_exn |>
        (fun x -> Vec.scale x 0.004) |>
        Vec.add t.center in

    let b =
      calc_tail_from_rot right_tail_rotate in

    let c =
      calc_tail_from_rot left_tail_rotate in
    Triangle.create a b c
    
  let bound t ~min ~max ~dim =
    let idx =
      match dim with
      | Axis.X -> 0
      | Y -> 1
      | Z -> 2 in
    let old_loc = t.center.(idx) in
    let new_loc =
      match Float.(<) old_loc min with
      | true -> max -. (min -. old_loc)
      | false ->
         match Float.(>) old_loc max with
         | true -> min +. (old_loc -. max)
         | false -> old_loc in
    t.center.(idx) <- new_loc

  let update_position min_x max_x min_y max_y t =
    let new_center =
      Vec.add
        t.center
        (Vec.scale t.norm_direction t.velocity) in
    t.center <- new_center;
    bound t ~min:min_x ~max:max_x ~dim:Axis.X;
    bound t ~min:min_y ~max:max_y ~dim:Axis.Y;
    t.homogeneous_center <- Vec.homogeneous_point t.center;
    t

  let update_direction t direction =
    let new_norm_direction, new_velocity =
      normalize_direction direction t.velocity in
    let new_homogeneous_norm_direction =
      Vec.homogeneous_vector new_norm_direction in
    t.norm_direction <- new_norm_direction;
    t.velocity <- new_velocity;
    t.homogeneous_norm_direction <- new_homogeneous_norm_direction;
    t
end                        
            

            

module Direction_sync = struct

  type t =
    {
      dir_grid : Vec.t Array.t Array.t;
      count_grid : int Array.t Array.t;
      width : int;
      height : int;
      trans_x : Float.t -> int;
      trans_y : Float.t -> int;
    }


  let create ~width ~height ~x_min ~x_max ~y_min ~y_max =
    let trans_x = (fun x -> Float.to_int (Helper.rescale ~low:x_min ~high:x_max ~new_low:0. ~new_high:(Float.of_int width) ~value:x)) in
    let trans_y = (fun y -> Float.to_int (Helper.rescale ~low:y_min ~high:y_max ~new_low:0. ~new_high:(Float.of_int height) ~value:y)) in
    {
      dir_grid = Array.make_matrix ~dimx:width ~dimy:height [|0.0; 0.0; 0.0|];
      count_grid = Array.make_matrix ~dimx:width ~dimy:height 0;
      width;
      height;
      trans_x;
      trans_y
    }

  let add_direction t ~row ~col vec =
    let row = if row < 0 then t.width - 1
              else row in
    let col = if col < 0 then t.height -1
              else col in
    let row = row mod t.width in
    let col = col mod t.height in
    t.dir_grid.(row).(col) <- Vec.add t.dir_grid.(row).(col) vec;
    t.count_grid.(row).(col) <- t.count_grid.(row).(col) + 1

  let add_boid t (boid : Boid.t) =
    let row = t.trans_x boid.center.(0) in
    let col = t.trans_y boid.center.(1) in
    for row_add = -1 to 1 do
      for col_add = -1 to 1 do
        add_direction t ~row:(row + row_add) ~col:(col + col_add) boid.norm_direction
      done
    done

  let adjusted_boid t (boid : Boid.t) =
    let row = t.trans_x boid.center.(0) in
    let col = t.trans_y boid.center.(1) in
    match t.count_grid.(row).(col) with
    | 0 -> boid
    | _ ->
       let area_direction = t.dir_grid.(row).(col) in
       match Vec.normalize area_direction with
       | Ok normalized ->
          Boid.update_direction boid (Vec.add (Vec.scale boid.norm_direction 0.99) (Vec.scale normalized 0.01))
       | Error _ -> boid


  let normalize_all t =
    for row = 0 to t.width - 1 do
      for col = 0 to t.height - 1 do
        match t.count_grid.(row).(col) with
        | 0 -> ()
        | _ ->
           match Vec.normalize t.dir_grid.(row).(col) with
           | Ok normalized ->
              t.dir_grid.(row).(col) <- normalized
           | Error _ ->
              ()
      done
    done

  let clear_all t =
    let t = {t with
              dir_grid = Array.make_matrix ~dimx:t.width ~dimy:t.height [|0.; 0.; 0.|];
              count_grid = Array.make_matrix ~dimx:t.width ~dimy:t.height 0} in
    t
end

type t = Boid.t array

let rand_boid () =
  let x = Helper.rand_between (Float.neg 1.0) 1.0 in
  let y = Helper.rand_between (Float.neg 1.0) 1.0 in
  let x_dir = Helper.rand_between (Float.neg 1.0) 1.0 in
  let y_dir = Helper.rand_between (Float.neg 1.0) 1.0 in
  let velocity = 0.005 in
  Boid.create [|x; y; 0.|] [|x_dir; y_dir; 0.|] velocity


  
let colors (boid : Boid.t) =
  let f value =
    (value +. 1.) /. 2. in
  let col =
    [|
      f boid.norm_direction.(0);
      f boid.norm_direction.(1);
      f boid.norm_direction.(2)|] in
  
  Triangle.create col col col
  


let create n =
  Array.of_list
    (Helper.f_list_to_n n ~f:(fun _idx -> rand_boid ()))


module Loop_state = struct
  type nonrec t = {
      mutable boids : t;
      mutable direction_sync : Direction_sync.t;
      mutable triangles : Basic_triangles.t
    }

  let create num_boids =
    let boids = create num_boids in
    let nth_triangle n =
      Boid.get_triangle boids.(n) in
    let nth_color n =
      colors boids.(n) in
    let triangles =
      Basic_triangles.create_fun
        num_boids
        nth_triangle
        nth_color in
    let direction_sync =
      Direction_sync.create
        ~width:30 ~height:30
        ~x_min:(Float.neg 1.0) ~x_max:1.0
        ~y_min:(Float.neg 1.0) ~y_max:1.0 in
    {boids; direction_sync; triangles}

  let draw_and_update _pid t _loop_state =
    Basic_triangles.draw t.triangles;
    Array.iter
      t.boids
      ~f:(Direction_sync.add_boid t.direction_sync);
    Direction_sync.normalize_all t.direction_sync;
    t.boids <-
      Array.map
        t.boids
        ~f:(Direction_sync.adjusted_boid t.direction_sync);
    t.direction_sync <-
      Direction_sync.clear_all t.direction_sync;
    let update_triangle idx =
      t.boids.(idx) <-
        Boid.update_position
          (Float.neg 1.0) 1.0 (Float.neg 1.0) 1.0 t.boids.(idx);
      Boid.get_triangle t.boids.(idx) in
    Helper.do_n_i
      (fun idx ->
        ignore (Basic_triangles.update
                  t.triangles
                  idx
                  (update_triangle idx)
                  Basic_triangles.Update.Vertex))
      (Array.length t.boids);
    let update_color idx =
      colors t.boids.(idx) in
    Helper.do_n_i
      (fun idx ->
        ignore (Basic_triangles.update
                  t.triangles
                  idx
                  (update_color idx)
                  Basic_triangles.Update.Color))
      (Array.length t.boids)
end

let create_program () =
  let%bind vid =
    Shaders.compile_shader
      Shaders.Standard.vertex_shader_color4 Gl.vertex_shader in
  let%bind fid =
    Shaders.compile_shader
      Shaders.Standard.fragment_shader Gl.fragment_shader in
  (*let%bind gid =
    Shaders.compile_shader
      Shaders.Multi_transform.geometry_shader Gl.geometry_shader in*)
  let pid = Gl.create_program () in
  let get_program pid e = Helper.get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid; Gl.delete_shader vid;
  Gl.attach_shader pid fid; Gl.delete_shader fid;
  (*Gl.attach_shader pid gid; Gl.delete_shader gid;*)
  (*Gl.bind_attrib_location pid 0 "transformation";*)
  Gl.bind_attrib_location pid 0 "vertex";
  Gl.bind_attrib_location pid 1 "color";
  Gl.link_program pid;
  
  if get_program pid Gl.link_status = Gl.true_
  then Ok pid
  else
    let len = get_program pid Gl.info_log_length in
    let log = Helper.get_string len (Gl.get_program_info_log pid len None) in
    (Gl.delete_program pid; Error (`Msg log))

let start () =
  let pid =
    match create_program () with
    | Ok pid -> pid
    | Error (`Msg msg) -> raise (Invalid_argument (Printf.sprintf "%s" msg)) in
  Random.init 0;
  (*Printf.printf "gets to start\n";
  let _state = Loop_state.create 10 in
  Printf.printf "creates state\n";*)
  (*Ok ()*)
  Main_loop.loop_draw
    ~draw:(Loop_state.draw_and_update pid)
    ~state_f:(fun () -> Loop_state.create 10000)
