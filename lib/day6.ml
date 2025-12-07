let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)
(*$=
  [[1; 2; 3]; [4; 5; 6]]  (transpose [[1; 4]; [2; 5]; [3; 6]])
*)

let prod xs = List.fold_left ( * ) 1 xs

let solve_problem1 = function
  | [] ->
      Printf.eprintf "Error: empty list";
      exit 1
  | op :: xs -> (
      let operands = List.map int_of_string xs in
      match op with
      | "+" -> General.sum_list operands
      | "*" -> prod operands
      | _ ->
          Printf.eprintf "Invalid operation: %s\n" op;
          exit 1)

let is_blank s = Str.string_match (Str.regexp " *$") s 0
(*$=
  false (is_blank "hi there")
  true  (is_blank "    ")
  false (is_blank "  hey  ")
  true  (is_blank "")
*)

let split_parts xs =
  let open List in
  fold_right
    (fun l acc -> if is_blank l then [] :: acc else (l :: hd acc) :: tl acc)
    xs [ [] ]
(*$=
  [["one";"two"]; ["three";"four"]] (split_parts ["one";"two";"  ";"three";"four"])
*)

let strip_last_chars xs =
  List.map (fun s -> Str.string_before s (String.length s - 1)) xs
(*$=
  ["on"; "tw"; "thre"]  (strip_last_chars ["one"; "two"; "three"])
*)

let solve_part part =
  match part with
  | [] ->
      Printf.eprintf "Error empty problem\n";
      exit 1
  | x :: _ -> (
      let operands =
        strip_last_chars part |> List.map String.trim |> List.map int_of_string
      in
      match Str.last_chars x 1 with
      | "+" -> General.sum_list operands
      | "*" -> prod operands
      | op ->
          Printf.eprintf "Invalid op %s\n" op;
          exit 1)

let solve input =
  Str.split (Str.regexp "\n") input
  |> List.map (fun line -> Str.split (Str.regexp " +") line)
  |> transpose |> List.map List.rev |> List.map solve_problem1
  |> General.sum_list
  |> Printf.printf "Part 1: %d\n";

  Str.split (Str.regexp "\n") input
  |> List.map (fun s -> String.to_seq s |> List.of_seq)
  |> transpose
  |> List.map (fun cs -> List.to_seq cs |> String.of_seq)
  |> split_parts |> List.map solve_part |> General.sum_list
  |> Printf.printf "Part 2: %d\n"
