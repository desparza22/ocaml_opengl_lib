open Tgl3
open! Mat_vec
open! Core
open Result.Let_syntax

module Field = struct
  type t = {
      name : string;
      floats : int
    }
end

let create_program (fields : Field.t list) vert geom_opt frag =
  let pid = Gl.create_program () in
  let get_program pid e =
    Helper.get_int (Gl.get_programiv pid e) in

  let%bind vid =
    match 
      Shaders.compile_shader
        vert
        Gl.vertex_shader with
    | Ok vid -> Ok vid
    | Error (`Msg msg) ->
       Error (`Msg (Printf.sprintf "vertex-shader: %s" msg)) 
  in
  Gl.attach_shader pid vid; Gl.delete_shader vid;
  
  let%bind () =
    match geom_opt with
    | Some geom ->
       let%map gid =
             match 
               Shaders.compile_shader
                 geom
                 Gl.geometry_shader with
             | Ok gid -> Ok gid
             | Error (`Msg msg) ->
                Error
                  (`Msg
                     (Printf.sprintf
                        "geometry-shader: %s" msg)) 
       in
       Gl.attach_shader pid gid; Gl.delete_shader gid
    | None -> Ok () in
        
  let%bind fid =
    match 
      Shaders.compile_shader
        frag
        Gl.fragment_shader with
    | Ok frag -> Ok frag
    | Error (`Msg msg) ->
       Error
         (`Msg
            (Printf.sprintf "fragment-shader: %s" msg)) 
  in
  Gl.attach_shader pid fid; Gl.delete_shader fid;
  
  List.iteri
    fields
    ~f:(fun idx field ->
      Gl.bind_attrib_location pid idx field.name);
  Gl.link_program pid;

  if get_program pid Gl.link_status = Gl.true_
  then Ok pid
  else
    let len = get_program pid Gl.info_log_length in
    let log =
      Helper.get_string
        len (Gl.get_program_info_log pid len None) in
    (Gl.delete_program pid; Error (`Msg log))



module Primitive = struct
  type t =
    Points
  | Lines
  | Triangles
end


module Data_container = struct
  type t = {
      mutable data : (float, Bigarray.float32_elt) Bigarray_wrapper.t;
      mutable data_capacity : int;
      floats_per_vertex : int;
      vertices_per_entry : int;
      floats_per_entry : int
    }

  let create (fields : Field.t list) primitive =
    let floats_per_vertex =
      List.fold
        fields
        ~init:0
        ~f:(fun accum field ->
          accum + field.floats) in
    let vertices_per_entry =
      match primitive with
      | Primitive.Points -> 1
      | Lines -> 2
      | Triangles -> 3 in
    let floats_per_entry =
      floats_per_vertex * vertices_per_entry in

    let data_capacity = 1000 in
    let data =
      Bigarray_wrapper.create
        Bigarray.float32
        (data_capacity * floats_per_entry) in
    {data; data_capacity; floats_per_vertex;
     vertices_per_entry; floats_per_entry}
    
end
                      
type t = {
    pid : int;
    primitive : Primitive.t;
    mutable num_primitives : int;
    fields : Field.t list;
    id : int;
    data_id : int;
    data : Data_container.t
  }
    


let create (fields : Field.t list) primitive vert geom_opt frag () =
  let pid =
    match create_program fields vert geom_opt frag with
    | Ok pid -> pid
    | Error (`Msg msg) -> 
       raise
         (Invalid_argument msg) in

  let store_res =
    Bigarray.Array1.create
      Bigarray.int32
      Bigarray.c_layout 1 in

  Gl.gen_vertex_arrays 1 store_res;
  let id = Int32.to_int_exn store_res.{0} in

  (*data*)
  Gl.gen_buffers 1 store_res;
  let data_id =
    Int32.to_int_exn store_res.{0} in
  Gl.bind_buffer Gl.array_buffer data_id;
  let data = Data_container.create fields primitive in 

  Gl.bind_vertex_array id;
  Gl.bind_buffer Gl.array_buffer data_id;
  ignore
    (List.foldi
       fields
       ~init:0
       ~f:(fun idx accum field ->
         Gl.enable_vertex_attrib_array idx;
         Gl.vertex_attrib_pointer
           idx
           field.floats
           Gl.float
           false
           (data.floats_per_vertex * 4)
           (`Offset accum);
         accum + field.floats * 4)
    : int);
      
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;

  {
    pid;
    primitive;
    num_primitives=0;
    fields;
    id;
    data_id;
    data
  }

let draw t =
  Gl.bind_buffer Gl.array_buffer t.data_id;

  Gl.buffer_data
    Gl.array_buffer
    (Gl.bigarray_byte_size t.data.data.data)
    (Some t.data.data.data)
    Gl.static_draw;
  
  let gl_flag =
    match t.primitive with
    | Primitive.Points -> Gl.points
    | Lines -> Gl.lines
    | Triangles -> Gl.triangles in
  Gl.use_program t.pid;
  Gl.bind_vertex_array t.id;
  Gl.draw_arrays
    gl_flag
    0
    (t.data.vertices_per_entry * t.num_primitives);
  Gl.bind_vertex_array 0

let clear t =
  t.num_primitives <- 0


let expand_capacity t =
  let new_capacity = t.data.data_capacity * 2 in
  let new_data =
    Bigarray_wrapper.create
      Bigarray.float32
      (new_capacity * t.data.floats_per_entry) in
  Helper.do_n_i
    (fun idx ->
      match Bigarray_wrapper.get t.data.data ~idx with
      | Ok elem ->
         ignore
           ((Bigarray_wrapper.set
              new_data
              ~idx
              ~elem)
           : (unit, Bigarray_wrapper.Error_type.t) result)
      | _ ->
         raise
           (Invalid_argument "shouldn't raise"))
    (t.num_primitives * t.data.floats_per_entry);

  t.data.data <- new_data;
  t.data.data_capacity <- new_capacity

let add_entry t getter =
  (if Int.equal t.num_primitives t.data.data_capacity
   then expand_capacity t);
  let idx = t.num_primitives in
  Helper.do_n_i
    (fun entry_i ->
      ignore
        ((Bigarray_wrapper.set
            t.data.data
            ~idx:(idx * t.data.floats_per_entry + entry_i)
            ~elem:(getter entry_i))
         : (unit, Bigarray_wrapper.Error_type.t) result)
    )
    t.data.floats_per_entry;
  t.num_primitives <- idx + 1


module Fancy_triangles = struct
  module Sub2 = struct
    type t = {
        num_triangles : int;
        id : int;
        indices_id : int;
        indices : (int, Bigarray.int16_unsigned_elt) Bigarray_wrapper.t;
        data_id : int;
        data : Data_container.t
      }

    let create num_vertices vertices_f colors_f
          num_triangles indices_f =
      let store_res = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in

      Gl.gen_vertex_arrays 1 store_res;
      
      let id = Int32.to_int_exn store_res.{0} in

      (* indices *)
      Gl.gen_buffers 1 store_res;
      let indices_id = Int32.to_int_exn store_res.{0} in
      Gl.bind_buffer Gl.array_buffer indices_id;
      let indices = Bigarray_wrapper.from_fun Bigarray.int16_unsigned (num_triangles * 3) indices_f in
      Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size indices.data) (Some indices.data) Gl.static_draw;

      (* data *)
      Gl.gen_buffers 1 store_res;
      let data_id =
        Int32
      
  end


  module Update = struct
    type t =
      | Vertex
      | Color
  end

  module Sub = struct
    type t = {
        num_triangles : int;
        id : int;
        indices_id : int;
        (* 0 to number of vertices minus 1 *)
        indices : (int, Bigarray.int16_unsigned_elt) Bigarray_wrapper.t;
        vertices_id : int;
        triangle_vertices : Triangle_array.t;
        colors_id : int;
        triangle_colors : Triangle_array.t
      }
           
    let create_fun num_triangles vertices_f colors_f indices_f =
      let store_res = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in

      Gl.gen_vertex_arrays 1 store_res;
      
      let id = Int32.to_int_exn store_res.{0} in

      (* indices *)
      Gl.gen_buffers 1 store_res;
      let indices_id = Int32.to_int_exn store_res.{0} in
      Gl.bind_buffer Gl.array_buffer indices_id;
      let indices = Bigarray_wrapper.from_fun Bigarray.int16_unsigned (num_triangles * 3) indices_f in
      Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size indices.data) (Some indices.data) Gl.static_draw;

      (* vertices *)
      Gl.gen_buffers 1 store_res;
      let vertices_id = Int32.to_int_exn store_res.{0} in
      Gl.bind_buffer Gl.array_buffer vertices_id;
      let vertices = Triangle_array.create num_triangles in
      Triangle_array.fill_f vertices vertices_f;
      Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertices.data.data.data) (Some vertices.data.data.data) Gl.dynamic_draw;

      (* colors *)
      Gl.gen_buffers 1 store_res;
      let colors_id = Int32.to_int_exn store_res.{0} in
      Gl.bind_buffer Gl.array_buffer colors_id;
      let colors = Triangle_array.create num_triangles in
      Triangle_array.fill_f colors colors_f;
      Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size colors.data.data.data) (Some colors.data.data.data) Gl.dynamic_draw;

      (* bind everything to id *)
      Gl.bind_vertex_array id;
      (* indices *)
      Gl.bind_buffer Gl.element_array_buffer indices_id;
      (* vertices *)
      Gl.bind_buffer Gl.array_buffer vertices_id;
      Gl.enable_vertex_attrib_array 0;
      Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
      (* colors *)
      Gl.bind_buffer Gl.array_buffer colors_id;
      Gl.enable_vertex_attrib_array 1;
      Gl.vertex_attrib_pointer 1 3 Gl.float false 0 (`Offset 0);

      (* reset bindings *)
      Gl.bind_vertex_array 0;
      Gl.bind_buffer Gl.array_buffer 0;
      Gl.bind_buffer Gl.element_array_buffer 0;

      (* return t *)
      {num_triangles; id;
       indices_id; indices;
       vertices_id; triangle_vertices = vertices;
       colors_id; triangle_colors = colors}


    let draw t program_id =
      (* vertices *)
      Gl.bind_buffer Gl.array_buffer t.vertices_id;
      Gl.buffer_data
        Gl.array_buffer
        (Gl.bigarray_byte_size
           t.triangle_vertices.data.data.data)
        (Some t.triangle_vertices.data.data.data)
        Gl.dynamic_draw;

      (* colors *)
      Gl.bind_buffer Gl.array_buffer t.colors_id;
      Gl.buffer_data
        Gl.array_buffer
        (Gl.bigarray_byte_size
           t.triangle_colors.data.data.data)
        (Some t.triangle_colors.data.data.data)
        Gl.dynamic_draw;
      
      Gl.use_program program_id;
      Gl.bind_vertex_array t.id;
      Gl.draw_elements
        Gl.triangles
        (3 * t.triangle_vertices.size)
        Gl.unsigned_short
        (`Offset 0);
      Gl.bind_vertex_array 0;
      ()
      
    let update t idx triangle update =
      let updating = 
        match update with
        | Update.Vertex -> t.triangle_vertices
        | Update.Color -> t.triangle_colors in
      Triangle_array.set updating ~idx ~triangle
      
  end

  (* indexed by shorts, at most ~65000 indices.
   three indices per triangle, so at most ~20000 triangles *)
  let triangles_per_subarray = 20000

  type t = {
      pid : int;
      num_triangles : int;
      sub_triangles : Sub.t array
    }
(*
  let create_program (fields : Field.t list) vert geom =
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
      *)


  let create_fun num_triangles vertices_f colors_f indices_f =
    let fields =
      [Field.{name="vertex"; floats=3};
       Field.{name="color"; floats=4}] in
    let vert = Shaders.Standard.vertex_shader_color4 in
    let geom_opt = None in
    let frag = Shaders.Standard.fragment_shader in
    match create_program fields vert geom_opt frag with
    | Error (`Msg msg) ->
       raise (Invalid_argument (Printf.sprintf "%s" msg))
    | Ok pid ->
       let num_subs =
         (num_triangles - 1) /
           triangles_per_subarray + 1 in
       let sub_triangles =
         Array.init
           num_subs
           ~f:(fun sub_num ->
             let sub_num_triangles =
               if Int.equal sub_num (num_subs - 1) &&
                    (not (Int.equal
                            (num_triangles mod
                               triangles_per_subarray)
                            0))
               then
                 num_triangles mod
                   triangles_per_subarray
               else
                 triangles_per_subarray in
             
             let start_idx =
               triangles_per_subarray * sub_num in
             let shifted_vertices_f =
               (fun idx ->
                 vertices_f (idx + start_idx)) in
             let shifted_colors_f =
               (fun idx ->
                 colors_f (idx + start_idx)) in
             let shifted_indices_f =
               (fun idx ->
                 indices_f (idx + start_idx)) in
             Sub.create_fun
               sub_num_triangles
               shifted_vertices_f
               shifted_colors_f
               shifted_indices_f) in
       {pid; num_triangles; sub_triangles}

  let create_list vertices_list =
    let num_triangles = List.length vertices_list in
    let vertices_array = List.to_array vertices_list in
    let vertices_f =
      (fun idx -> vertices_array.(idx)) in
    create_fun num_triangles vertices_f

  let draw t =
    Array.iter
      t.sub_triangles
      ~f:(fun sub -> Sub.draw sub t.pid)

  let update t idx triangle update =
    let sub_num = idx / triangles_per_subarray in
    let idx_in_sub = idx mod triangles_per_subarray in
    Sub.update
      t.sub_triangles.(sub_num)
      idx_in_sub
      triangle
      update

end
