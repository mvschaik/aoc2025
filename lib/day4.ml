let print_map world =
  let min_row, max_row, min_col, max_col =
    Hashtbl.fold
      (fun (row, col) _ (min_row, max_row, min_col, max_col) ->
        (min row min_row, max row max_row, min col min_col, max col max_col))
      world
      (max_int, min_int, max_int, min_int)
  in
  for row = min_row to max_row do
    for col = min_col to max_col do
      match Hashtbl.find_opt world (row, col) with
      | None -> Printf.printf "�"
      | Some c -> Printf.printf "%c" c
    done;
    Printf.printf "\n"
  done

let parse_map input : (int * int, char) Hashtbl.t =
  Str.split (Str.regexp "\n") input
  |> List.to_seq
  |> Seq.mapi (fun row line ->
      String.to_seq line |> Seq.mapi (fun col c -> ((row, col), c)))
  |> Seq.concat |> Hashtbl.of_seq

let rec get_somes xs =
  match xs with
  | [] -> []
  | None :: rest -> get_somes rest
  | Some x :: rest -> x :: get_somes rest
(*$=
  [1; 2]  (get_somes [None; Some 1; None; Some 2; None])
*)

let neighbors (row, col) map =
  [
    Hashtbl.find_opt map (row - 1, col - 1);
    Hashtbl.find_opt map (row - 1, col);
    Hashtbl.find_opt map (row - 1, col + 1);
    Hashtbl.find_opt map (row, col - 1);
    Hashtbl.find_opt map (row, col + 1);
    Hashtbl.find_opt map (row + 1, col - 1);
    Hashtbl.find_opt map (row + 1, col);
    Hashtbl.find_opt map (row + 1, col + 1);
  ]
  |> get_somes

let can_be_removed pos world =
  let rolls = neighbors pos world |> List.filter (fun c -> c = '@') in
  List.length rolls < 4

let rec clear_rolls world =
  let to_remove =
    Hashtbl.to_seq world
    |> Seq.filter (fun (pos, c) -> c = '@' && can_be_removed pos world)
    |> List.of_seq
  in
  let num_to_remove = List.length to_remove in
  if num_to_remove = 0 then 0
  else begin
    List.iter (fun (pos, _) -> Hashtbl.replace world pos 'x') to_remove;
    num_to_remove + clear_rolls world
  end

let solve input =
  let world = parse_map input in
  let num_reachable =
    Hashtbl.to_seq world
    |> Seq.filter (fun (pos, c) -> c = '@' && can_be_removed pos world)
    |> Seq.length
  in
  Printf.printf "Part 1: %d\n" num_reachable;
  Printf.printf "Part 2: %d\n" (clear_rolls world)
