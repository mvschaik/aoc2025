let merge_ranges (b1, e1) (b2, e2) =
  if e1 < b2 || e2 < b1 then None else Some (min b1 b2, max e1 e2)
(*$=
  None              (merge_ranges (40, 50) (20, 30))
  (Some (30, 60))   (merge_ranges (30, 50) (40, 60))
  (Some (30, 60))   (merge_ranges (30, 60) (40, 50))
  (Some (20, 30))   (merge_ranges (20, 30) (20, 30))
*)

let rec merge_range_list ranges =
  match ranges with
  | [] -> []
  | r :: rest ->
      let new_ranges, is_merged =
        List.fold_left
          (fun (acc, is_merged) r2 ->
            match merge_ranges r r2 with
            | None -> (r2 :: acc, is_merged)
            | Some rr -> (rr :: acc, true))
          ([], false) rest
      in
      if is_merged then merge_range_list new_ranges
      else r :: merge_range_list rest

let range_size (s, e) = e - s + 1

let solve input =
  let parts = Str.split (Str.regexp "\n\n") input in
  let ranges =
    Str.split (Str.regexp "\n") (List.hd parts)
    |> List.map (fun s -> Scanf.sscanf s "%d-%d" (fun s e -> (s, e)))
  in
  let ingredients =
    Str.split (Str.regexp "\n") (List.nth parts 1) |> List.map int_of_string
  in
  let fresh =
    ingredients
    |> List.filter (fun i -> List.exists (fun (s, e) -> i > s && i <= e) ranges)
  in

  Printf.printf "Part 1: %d\n" (List.length fresh);

  Printf.printf "Part 2: %d\n"
    (merge_range_list ranges |> List.map range_size |> General.sum_list)
