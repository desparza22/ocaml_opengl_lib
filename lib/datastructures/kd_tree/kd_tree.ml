
type ('a, 'b) t =
  {
    axis : int;
    num_axes : int;
    get_axis_val : 'a -> int -> 'b;
    subtract_axis_vals : 'b -> 'b -> Float.t;
    dist_between_elems : 'a -> 'a -> Float.t;
    lower : (('a, 'b) t * 'b) option;
    higher : (('a, 'b) t * 'b) option;
    data : 'a option
  }

    
let rec rec_create ~current_axis ~num_axes
          ~(get_axis_val : 'a -> int -> 'b) ~subtract_axis_vals
          ~dist_between_elems ~data  : ('a, 'b) t =
  let (data, lower, higher) =
    if Array.length data <= 1
    then
      let data =
        match Array.length data with
        | 0 -> raise (Invalid_argument "kd tree splitting shouldn't have allowed 0 element data array. did you create an empty kd_tree?")
        | _ -> Some data.(0) in
      (data, None, None)
    else
      (Array.sort
         (fun a b ->
           Float.compare
             (subtract_axis_vals
                (get_axis_val a current_axis)
                (get_axis_val b current_axis))
             0.)
        data;
       let median = Array.length data / 2 in
       let left_high =
         get_axis_val data.(median - 1) current_axis in
       let right_low =
         get_axis_val data.(median) current_axis in
       let left_data =
         Array.init
           median
           (fun idx -> data.(idx)) in
       let right_data =
         Array.init
           (Array.length data - median)
           (fun idx -> data.(idx + median)) in
       let rec_create_help child_data =
         rec_create
           ~current_axis:((current_axis + 1) mod num_axes)
           ~num_axes
           ~get_axis_val
           ~subtract_axis_vals
           ~dist_between_elems
           ~data:child_data in
       let lower = Some (rec_create_help left_data, left_high) in
       let higher = Some (rec_create_help right_data, right_low) in
       let data = None in
       (data, lower, higher)) in
  {axis = current_axis;
   num_axes;
   get_axis_val;
   subtract_axis_vals;
   dist_between_elems;
   lower;
   higher;
   data}
    

let create ~num_axes =
  rec_create ~current_axis:0 ~num_axes

let farthest_distance t target (closest_found : 'a Sorted.t) =
  match closest_found.num_elems with
  | 0 -> None
  | n ->
     let last_idx = n - 1 in
     let farthest_elem = closest_found.data.(last_idx) in
     let distance =
       t.dist_between_elems target farthest_elem in
     Some distance

let rec rec_nearest_k t target k closest_found =
  match t.data with
  | Some elem ->
     Sorted.insert closest_found elem
  | None ->
     let farthest_distance_found =
       farthest_distance t target closest_found in
     (match t.lower with
     | None -> ()
     | Some (lower_tree, highest_value) ->
        let distance_down =
          t.subtract_axis_vals
            (t.get_axis_val target t.axis)
            highest_value in
        if
          match farthest_distance_found with
          | None -> true
          | Some farthest ->
             Helper.float_lt distance_down farthest
        then
          rec_nearest_k lower_tree target k closest_found);
     let farthest_distance_found =
       farthest_distance t target closest_found in
     (match t.higher with
      | None -> ()
      | Some (higher_tree, lowest_value) ->
         let distance_up =
           t.subtract_axis_vals
             lowest_value
             (t.get_axis_val target t.axis) in
         if
           match farthest_distance_found with
           | None -> true
           | Some farthest ->
              Helper.float_lt distance_up farthest
         then
           rec_nearest_k higher_tree target k closest_found)
                

           

let nearest_k t target k =
  let closest_found =
    Sorted.create
      ~size:k
      ~witness:target
      ~comparer:(fun a b -> Float.compare (t.dist_between_elems target a) (t.dist_between_elems target b)) in
  rec_nearest_k t target k closest_found;
  closest_found
