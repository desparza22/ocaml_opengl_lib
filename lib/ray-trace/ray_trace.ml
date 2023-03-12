open Tgl3
open Mat_vec
open! Core
open Result.Let_syntax

let _background_color (ray : Ray.t) =
  let lerp_v = 0.5 *. (ray.norm_direction.(1) +. 1.) in
  Vec.add
    (Vec.scale [|0.4; 0.5; 0.8|] lerp_v)
    (Vec.scale [|0.05; 0.1; 0.2|] (1. -. lerp_v))

let background_color _ =
  Vec.scale Scene.white 0.6

let photon_light
      (scene : Scene.t)
      (record : Hittables.Hit_record.t)
      (surface : Hittables.Surface.t) =
  (* huuuge code smell. to search a kd tree of light_sources, we need a light
     source. so convert the point we are searching into a light source. this should
     be corrected from the kd_tree *)
  let dummy_light_source =
    Scene.Light_source.create
      false record surface (Ray.create ~start:[|0.; 0.; 0.|] ~direction:[|1.; 1.; 1.|]) ([|0.; 0.; 0.|]) in
  let nearest_photons =
    Kd_tree.nearest_k
      scene.photon_map
      dummy_light_source
      10 in
  let nearest_photons =
    Array.to_list nearest_photons.data in
  List.fold
    nearest_photons
    ~f:(fun contributions light_source ->
      let distance =
        Vec.magnitude_sqrd (Vec.sub light_source.hit_record.hit_point record.hit_point) in
      let distance =
        Helper.clamp distance ~low:0. ~high:0.008 in
      let contribution =
        Vec.scale Scene.white ((0.008 -. distance) *. 100.) in
      Vec.add contributions contribution
    )
    ~init:Scene.black
          
  

let direct_light
      (scene : Scene.t)
      (record : Hittables.Hit_record.t)
      (surface : Hittables.Surface.t)
      _debug =
  let light_contribution =
    List.fold
      scene.light_sources
      ~f:(fun contributions light_source ->
        let (on_source1, on_source2) = light_source.random_points () in
        let (vec_to_light1, vec_to_light2) =
          (Vec.sub on_source1 record.hit_point,
           Vec.sub on_source2 record.hit_point) in
        let (dist_to_light1, dist_to_light2) =
          (Vec.magnitude_sqrd vec_to_light1,
           Vec.magnitude_sqrd vec_to_light2) in
        let (vec_to_light, dist_to_light) =
          if Helper.float_lt dist_to_light1 dist_to_light2
          then (vec_to_light1, dist_to_light1)
          else (vec_to_light2, dist_to_light2) in
        let ray =
          Ray.create
            ~start:record.hit_point
            ~direction:vec_to_light in
        let hit_record =
          Hittables.intersection
            scene.hittables ray ~dist_min:0.00001 ~dist_max:30. in
        let hit_contribution () =
          let contrib =
            Hittables.Surface.Material.reflection_probability_density
              surface.material
              ~normal:record.normal
              ~light_out:record.normal
              ~light_in_flipped:ray.norm_direction
          in
          contrib
        in
        let contribution =
          match hit_record with
          | Some (intersect_record, _) ->
             let dist_to_intersect =
               Vec.magnitude_sqrd
                 (Vec.sub
                    intersect_record.hit_point
                    record.hit_point) in
             if
               Helper.float_lt
                 (dist_to_intersect +. 0.0001)
                 dist_to_light
             then 0.
             else hit_contribution ()
          | None -> hit_contribution () in
        contributions +. contribution)
      ~init:0. in
  Vec.scale
    Scene.white
    (light_contribution /. (Float.of_int scene.num_light_sources))


let _ray_color_by_normal
      (ray : Ray.t)
      (scene : Scene.t)
      ~dist_min
      ~dist_max =
    let normal =
      if Helper.float_lte (Helper.rand_between 0. 1.) 0.5
      then Vec.normalize_exn ([|0.; 1.; 1.|])
    else                          
      let hit_record =
        Hittables.intersection
          scene.hittables ray ~dist_min ~dist_max in
      match hit_record with
      | Some (record, _surface) ->
         record.normal
      | None ->
         [|0.; 0.; 1.|] in
  Vec.add ([|1.; 1.; 1.|]) normal
  |> (fun col -> Vec.scale col 0.5)

let _ray_color_by_direct_light
      (ray : Ray.t)
      (scene : Scene.t)
      ~dist_min
      ~dist_max =
  let hit_record =
        Hittables.intersection
          scene.hittables ray ~dist_min ~dist_max in
  match hit_record with
  | Some (record, surface) ->
     (match surface.material with
     | Light -> Scene.red
     | _ ->
        Vec.mult Scene.blue (direct_light scene record surface false))
  | None -> Scene.black

let _ray_color_by_surface
      (ray : Ray.t)
      (scene : Scene.t)
      ~dist_min
      ~dist_max =
  let hit_record =
        Hittables.intersection
          scene.hittables ray ~dist_min ~dist_max in
  match hit_record with
  | Some (_record, surface) ->
     (match surface.material with
      | Light -> Scene.white
      | Metal -> Scene.red
      | Diffuse -> Scene.green
      | Water -> Scene.blue)
  | None -> Scene.black
       

          
let rec ray_color
          (ray : Ray.t)
          (scene : Scene.t)
          ~dist_min
          ~dist_max =
  let hit_record =
        Hittables.intersection
          scene.hittables ray ~dist_min ~dist_max in
  match hit_record with
  | Some (record, surface) ->
     (match surface.material with
     | Light -> Scene.white
     | _ ->
        let debug = match surface.material with
          | Metal -> true
          | _ -> false in
        (let direct_in = direct_light scene record surface debug in
         let roulette_probability =
           Hittables.Surface.Material.roulette_probability
             surface.material in
         if
           Helper.float_lt
             (Helper.rand_between 0. 1.) roulette_probability
         then
           let flipped_light_in_ray_opt =
             Hittables.Surface.scatter_and_attenuation
               surface
               ~ray_in:ray
               ~hit_record:record in
           let (in_color, in_pdf) =
             match flipped_light_in_ray_opt with
             | Some flipped_light_in_ray ->
                (ray_color
                  flipped_light_in_ray
                  scene
                  ~dist_min:0.00001 ~dist_max:30.0,
                 Hittables.Surface.Material.reflection_probability_density
                   surface.material
                   ~normal:record.normal
                   ~light_out:(Vec.neg ray.norm_direction)
                   ~light_in_flipped:flipped_light_in_ray.norm_direction)
             | None ->
                ([|0.; 0.; 0.|], 1.) in
           match surface.material with
           | Metal ->
              (*Printf.printf
                "in_pdf: %f roulette_prob: %f\n"
                in_pdf
                roulette_probability;
              Printf.printf
                "direct_in: (%f %f %f) in_color: (%f %f %f)\n"
                direct_in.(0) direct_in.(1) direct_in.(2)
                in_color.(0) in_color.(1) in_color.(2);*)
              Vec.add
                (Vec.mult direct_in surface.color)
                (Vec.scale in_color (in_pdf /. roulette_probability))
           | _ ->
              Vec.add
                direct_in
                (Vec.scale in_color (in_pdf /. roulette_probability))
              |>
                Vec.mult surface.color
         else
           Vec.mult direct_in surface.color))
  | None ->
     background_color ray


    
let ray_color2
          (ray : Ray.t)
          (scene : Scene.t)
          ~dist_min
          ~dist_max =
  let hit_record =
        Hittables.intersection
          scene.hittables ray ~dist_min ~dist_max in
  match hit_record with
  | Some (record, surface) ->
     (match surface.material with
     | Light -> Scene.white
     | _ ->
        let ray_color_contrib =
          ray_color ray scene ~dist_min ~dist_max in
        let photon_light_contrib = 
          photon_light
            scene
            record
            surface in
        (Vec.add
           (Vec.scale ray_color_contrib 0.7)
           (Vec.scale photon_light_contrib 0.1)))
  | None ->
     background_color ray
          
type t =
  {
    mutable iteration : int;
    mutable triangles : Basic_triangles.t;
    idx_permutation : int array;
    camera : Camera.t;
    screen : Screen.t
  }

let box_coor ~n ~width =
  let is_top = Int.equal (n mod 2) 0 in
  let box_num = n / 2 in
  let x = box_num mod width in
  let y = box_num / width in
  (is_top, x, y)

let triangle_nums ~x ~y ~width =
  let box_num = y * width + x in
  let n = box_num * 2 in
  (n, n+1)
      


let num_triangles screen =
  (Screen.width screen) * (Screen.height screen) * 2

let nth_triangle screen =
  let width = Screen.width screen in
  let height = Screen.height screen in
  let box_width = 2. /. (Float.of_int width) in
  let box_height = 2. /. (Float.of_int height) in
  (fun n ->
    let (is_top, x, y) = box_coor ~n ~width in
    let left = (Float.neg 1.) +. (Float.of_int x) *. box_width in
    let top = (Float.neg 1.) +. (Float.of_int y) *. box_height in
    let right = left +. box_width in
    let bottom = top +. box_height in
    if is_top
    then Triangle.create [|left; top; 0.|] [|right; top; 0.|] [|left; bottom; 0.|]
    else Triangle.create [|left; bottom; 0.|] [|right; bottom; 0.|] [|right; top; 0.|]
  )

let _normalize_except_black color =
  match Vec.normalize color with
  | Ok normalized -> normalized
  | Error _ -> Scene.black
  
let nth_color screen n =
  let width = Screen.width screen in
  let (_, x, y) = box_coor ~n ~width in
  let color = Screen.get screen ~x ~y in
  let gamma_corrected_color =
    Array.map color ~f:Float.sqrt in
  Triangle.create gamma_corrected_color gamma_corrected_color gamma_corrected_color
  
let nth_color_set scene t n =
  let width = Screen.width t.screen in
  let (_, x, y) = box_coor ~n ~width in
  let ray = Camera.ray_to t.camera ~x ~y ~antialias:true in
  let color = ray_color2 ray scene ~dist_min:0. ~dist_max:30. in
  Screen.add t.screen ~x ~y color

  

let draw_and_update _pid t _loop_state =
  let scene = Scene.scene2 in
  let pixels_per_iter = 5000 in
  (* mod here to prevent overflow *)
  let start_idx =
    (t.iteration mod t.triangles.num_triangles) *
      pixels_per_iter in
  Helper.do_n_i
    (fun i ->
      let permutation_idx = (i + start_idx) mod t.triangles.num_triangles in
      let permuted_idx = t.idx_permutation.(permutation_idx) in
      nth_color_set scene t permuted_idx)
    pixels_per_iter;
  let frame_print_rate = 1 in
  if Int.equal (t.iteration mod frame_print_rate) 0
  then
    let filename =
      Printf.sprintf
        "video/image%d.ppm"
        (t.iteration / frame_print_rate) in
    Screen.to_ppm_file t.screen filename
  else ();
  let first_buffer = Int.equal (t.iteration mod 2) 0 in
  let to_update = Screen.updated t.screen first_buffer in
  Screen.clear_updated t.screen first_buffer;
  List.iter
    to_update
    ~f:(fun (x, y) ->
      let width = Screen.width t.screen in
      let (pos0, pos1) = triangle_nums ~x ~y ~width in
      ignore (Basic_triangles.update
                t.triangles
                pos0
                (nth_color t.screen pos0)
                Basic_triangles.Update.Color);
      ignore (Basic_triangles.update
                t.triangles
                pos1
                (nth_color t.screen pos1)
                Basic_triangles.Update.Color);
    );
  Basic_triangles.draw t.triangles;
  t.iteration <- t.iteration + 1
  

let state_create () =
  let aspect_ratio = 16. /. 9. in
  let width_pix = 400 in
  let height_pix = Float.to_int ((Float.of_int width_pix) /. aspect_ratio) in
  let focal_length = 1.0 in
  let height_world = 2.0 in
  let width_world = height_world *. aspect_ratio in
  let camera =
    Camera.create
      ~focal_length ~width_world ~height_world ~width_pix ~height_pix in
  let pixel_color ~x ~y =
    let ray = Camera.ray_to camera ~x ~y ~antialias:true in
     background_color ray in
  let screen =
    Screen.create ~width:width_pix ~height:height_pix pixel_color in
  let triangles =
    Basic_triangles.create_fun
      (num_triangles screen)
      (nth_triangle screen)
      (nth_color screen) in
  let idx_permutation = Helper.permutation ~low:0 ~high:(triangles.num_triangles - 1) in
  {iteration=0;
   triangles;
   idx_permutation;
   camera;
   screen}
      
let create_program () =
  let%bind vid =
    Shaders.compile_shader
      Shaders.Standard.vertex_shader_color4 Gl.vertex_shader in
  let%bind fid =
    Shaders.compile_shader
      Shaders.Standard.fragment_shader Gl.fragment_shader in
  let pid = Gl.create_program () in
  let get_program pid e = Helper.get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid; Gl.delete_shader vid;
  Gl.attach_shader pid fid; Gl.delete_shader fid;
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
  Main_loop.loop_draw
    ~draw:(draw_and_update pid)
    ~state_f:state_create
