let make_triple s =
  String.split_on_char ',' s |> List.map int_of_string |> fun xs ->
  match xs with
  | [ x; y; z ] -> (x, y, z)
  | _ ->
      Printf.eprintf "Invalid position";
      exit 1
(*$=
  (1, 2, 3) (make_triple "1,2,3")
*)

let square x = x * x

let dist (x1, y1, z1) (x2, y2, z2) =
  square (Int.abs (x1 - x2))
  + square (Int.abs (y1 - y2))
  + square (Int.abs (z1 - z2))

class uf size =
  object
    val parent = Array.init size Fun.id
    val count = ref size
    method find a = parent.(a)

    method union a b =
      let a' = parent.(a) in
      let b' = parent.(b) in
      if a' <> b' then begin
        Array.map_inplace (fun p -> if p = a' then b' else p) parent;
        decr count
      end

    method connected a b = parent.(a) = parent.(b)
    method count = !count

    method sizes =
      Array.fold_left
        (fun acc a ->
          acc.(a) <- acc.(a) + 1;
          acc)
        (Array.make (Array.length parent) 0)
        parent
      |> Array.to_list |> List.sort compare |> List.rev
  end

let solve input =
  let boxes =
    String.trim input |> String.split_on_char '\n' |> List.map make_triple
  in
  let num_conn = if List.length boxes = 20 then 10 else 1000 in
  let distances =
    List.mapi
      (fun i a ->
        List.take (i - 1) boxes |> List.mapi (fun j b -> ((i, j), dist a b)))
      boxes
    |> List.concat
    |> List.sort (fun (_, d1) (_, d2) -> compare d1 d2)
  in
  let conn = new uf (List.length boxes) in

  List.iter (fun ((a, b), _) -> conn#union a b) (List.take num_conn distances);
  List.take 3 conn#sizes |> List.fold_left ( * ) 1
  |> Printf.printf "Part 1: %d\n";

  List.drop num_conn distances
  |> List.to_seq
  |> Seq.drop_while (fun ((a, b), _) ->
      conn#union a b;
      conn#count <> 1)
  |> Seq.take 1
  |> Seq.iter (fun ((a, b), _) ->
      let ax, _, _ = List.nth boxes a in
      let bx, _, _ = List.nth boxes b in
      Printf.printf "Part 2: %d\n" (ax * bx))
