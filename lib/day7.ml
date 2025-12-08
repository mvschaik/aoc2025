let up (row, col) = (row - 1, col)
let left (row, col) = (row, col - 1)
let right (row, col) = (row, col + 1)
let down (row, col) = (row + 1, col)

let rec is_connected world pos =
  match Hashtbl.find_opt world pos with
  | Some 'S' -> true
  | Some '^' -> false
  | Some '.' -> (
      is_connected world (up pos)
      || (match Hashtbl.find_opt world (left pos) with
        | Some '^' -> is_connected world (up (left pos))
        | _ -> false)
      ||
      match Hashtbl.find_opt world (right pos) with
      | Some '^' -> is_connected world (up (right pos))
      | _ -> false)
  | _ -> false

let find_start world =
  match
    Seq.ints 0 |> Seq.find (fun col -> Hashtbl.find world (0, col) = 'S')
  with
  | None ->
      Printf.eprintf "Can't find starting point\n";
      exit 1
  | Some col -> (0, col)

let memo_rec f =
  let h = Hashtbl.create 16 in
  let g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f x in
      Hashtbl.add h x y;
      y
  in
  g

let rec num_splits =
  let num_splits_inner world pos =
    match Hashtbl.find_opt world pos with
    | Some '^' -> 1 + num_splits world (left pos) + num_splits world (right pos)
    | Some '.' -> num_splits world (down pos)
    | _ -> 0
  in
  memo_rec num_splits_inner

let solve input =
  let world = General.parse_map input in
  Hashtbl.fold
    (fun pos v acc ->
      acc + if v = '^' && is_connected world (up pos) then 1 else 0)
    world 0
  |> Printf.printf "Part 1: %d\n";

  let start = find_start world in
  Printf.printf "Part 2: %d\n" (num_splits world (down start))
