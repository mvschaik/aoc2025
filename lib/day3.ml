let list_of_num s =
  String.to_seq s
  |> Seq.map (fun c -> int_of_string @@ String.make 1 c)
  |> List.of_seq
(*$=
  [1; 2; 3]  (list_of_num "123")
*)

let num_of_list s = List.fold_left (fun a n -> (10 * a) + n) 0 s
(*$=
  123   (num_of_list [1; 2; 3])
*)

let rec maxi ?(at = 0) xs =
  match xs with
  | [] -> (-1, -1)
  | x :: rest ->
      let largest_rest = maxi ~at:(at + 1) rest in
      if fst largest_rest > x then largest_rest else (x, at)
(*$=
  (-1, -1)  (maxi [])
  (3, 0)    (maxi [3])
  (5, 0)    (maxi [5; 3])
  (5, 1)    (maxi [3; 5])
  (7, 1)    (maxi [3; 7; 5])
*)

let rec make_largest num_digits nums =
  match num_digits with
  | 0 -> []
  | n ->
      let mx, i = maxi (List.take (List.length nums - n + 1) nums) in
      mx :: make_largest (num_digits - 1) (List.drop (i + 1) nums)

let solve_line num_digits l =
  list_of_num l |> make_largest num_digits |> num_of_list

let solve lines =
  List.map (solve_line 2) lines
  |> General.sum_list
  |> Printf.printf "Part 1: %d\n";
  List.map (solve_line 12) lines
  |> General.sum_list
  |> Printf.printf "Part 2: %d\n"
