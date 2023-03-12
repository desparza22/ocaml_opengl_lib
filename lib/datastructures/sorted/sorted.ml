type 'a t =
  {data : 'a Array.t;
   mutable num_elems : int;
   comparer : 'a -> 'a -> int}

let create ~size ~witness ~comparer =
  let data = Array.make size witness in
  {data; num_elems = 0; comparer}

let rec insert_from t elem index =
  if index == t.num_elems
  then
    if index == Array.length t.data
    then ()
    else (t.data.(index) <- elem; t.num_elems <- t.num_elems + 1)
  else
    let other = t.data.(index) in
    if t.comparer elem other < 0
    then
      (t.data.(index) <- elem;
       insert_from t other (index + 1))
    else
      insert_from t elem (index + 1)

let insert t elem =
  insert_from t elem 0

let get t index =
  if t.num_elems <= index
  then None
  else Some t.data.(index)
