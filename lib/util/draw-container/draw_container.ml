open Tgl3
open! Mat_vec
open! Core
open Result.Let_syntax


module Shared = struct
  module Field = struct
    type t = {
        name : string;
        floats : int
      }
  end

  let create_program
        (fields : Field.t list) vert geom_opt frag =
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
         Error
           (`Msg (Printf.sprintf "vertex-shader: %s" msg)) 
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
        mutable data :
                  (float, Bigarray.float32_elt)
                    Bigarray_wrapper.t;
        mutable data_capacity : int;
        mutable num_entries : int;
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
      {data; data_capacity; num_entries = 0;
       floats_per_vertex; vertices_per_entry;
       floats_per_entry}

    let expand_capacity t =
      let new_capacity = t.data_capacity * 2 in
      let new_data =
        Bigarray_wrapper.create
          Bigarray.float32
          (new_capacity * t.floats_per_entry) in
      Helper.do_n_i
        (fun idx ->
          match Bigarray_wrapper.get t.data ~idx with
          | Ok elem ->
             ignore
               ((Bigarray_wrapper.set
                   new_data
                   ~idx
                   ~elem)
                : (unit, Bigarray_wrapper.Error_type.t)
                    result)
          | _ ->
             raise
               (Invalid_argument "shouldn't raise"))
        (t.num_entries * t.floats_per_entry);

      t.data <- new_data;
      t.data_capacity <- new_capacity


    let add_entry t getter =
      (if Int.equal t.num_entries t.data_capacity
       then expand_capacity t);
      let idx = t.num_entries in
      Helper.do_n_i
        (fun entry_i ->
          ignore
            ((Bigarray_wrapper.set
                t.data
                ~idx:(idx * t.floats_per_entry + entry_i)
                ~elem:(getter entry_i))
             : (unit, Bigarray_wrapper.Error_type.t) result)
        )
        t.floats_per_entry;
      t.num_entries <- idx + 1 

    let clear t =
      t.num_entries <- 0
  end

  module Draw = struct

    type t =
      Basic of Primitive.t
    | Fancy

    let draw t pid id data_id
          (data_container : Data_container.t) =
      Gl.bind_buffer Gl.array_buffer data_id;

      Gl.buffer_data
        Gl.array_buffer
        (Gl.bigarray_byte_size data_container.data.data)
        (Some data_container.data.data)
        Gl.static_draw;

      Gl.use_program pid;
      Gl.bind_vertex_array id;
      (match t with
       | Basic primitive ->
          let gl_flag =
            match primitive with
            | Primitive.Points -> Gl.points
            | Lines -> Gl.lines
            | Triangles -> Gl.triangles in
          Gl.draw_arrays
            gl_flag
            0
            (data_container.vertices_per_entry *
               data_container.num_entries)
       | Fancy ->
          Gl.draw_elements
            Gl.triangles
            data_container.num_entries
            Gl.unsigned_short
            (`Offset 0));
      Gl.bind_vertex_array 0      
  end
end
              
module Basic = struct 
  type t = {
      pid : int;
      primitive : Shared.Primitive.t;
      fields : Shared.Field.t list;
      id : int;
      data_id : int;
      data : Shared.Data_container.t
    }

  let create (fields : Shared.Field.t list) primitive
        vert geom_opt frag =
    let pid =
      match Shared.create_program
              fields vert geom_opt frag with
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
    let data_id = Int32.to_int_exn store_res.{0} in
    Gl.bind_buffer Gl.array_buffer data_id;
    let data =
      Shared.Data_container.create fields primitive in 

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
      fields;
      id;
      data_id;
      data
    }

  let draw t =
    let draw_t = Shared.Draw.Basic t.primitive in
    Shared.Draw.draw
      draw_t
      t.pid
      t.id
      t.data_id
      t.data

  let clear t =
    Shared.Data_container.clear t.data

  let add_entry t getter =
    Shared.Data_container.add_entry t.data getter

  let floats_per_vertex t = t.data.floats_per_vertex

end        


module Fancy = struct
  let max_vertices = Int.shift_left 1 16
                   
  type t = {
      pid : int;
      num_triangles : int;
      id : int;
      indices_id : int;
      indices :
        (int, Bigarray.int16_unsigned_elt)
          Bigarray_wrapper.t;
      data_id : int;
      data : Shared.Data_container.t
    }

  let create ~fields ~vert ~geom_opt ~frag
        ~num_vertices ~getter
        ~num_triangles ~indices_f =
    if num_vertices > max_vertices
    then
      raise (Invalid_argument "excedes max vertices")
    else
      let pid =
        match
          Shared.create_program fields vert geom_opt frag
        with
        | Error (`Msg msg) ->
           raise (Invalid_argument (Printf.sprintf "%s" msg))
        | Ok pid -> pid in
      let store_res =
        Bigarray.Array1.create
          Bigarray.int32 Bigarray.c_layout 1 in

      Gl.gen_vertex_arrays 1 store_res;
      
      let id = Int32.to_int_exn store_res.{0} in

      (* indices *)
      Gl.gen_buffers 1 store_res;
      let indices_id = Int32.to_int_exn store_res.{0} in
      Gl.bind_buffer Gl.array_buffer indices_id;
      let indices =
        Bigarray_wrapper.from_fun
          Bigarray.int16_unsigned
          (num_triangles * 3) indices_f in
      Gl.buffer_data
        Gl.array_buffer
        (Gl.bigarray_byte_size indices.data)
        (Some indices.data) Gl.static_draw;

      (* data *)
      Gl.gen_buffers 1 store_res;
      let data_id = Int32.to_int_exn store_res.{0} in
      Gl.bind_buffer Gl.array_buffer data_id;
      let data =
        Shared.Data_container.create
          fields Shared.Primitive.Points in
      (Helper.do_n_i
         (fun idx ->
           Shared.Data_container.add_entry
             data (getter idx))
         num_vertices);

      {pid; num_triangles; id; indices_id; indices;
       data_id; data}


  let draw t =
    let draw_t = Shared.Draw.Fancy in
    Shared.Draw.draw
      draw_t
      t.pid
      t.id
      t.data_id
      t.data
end

