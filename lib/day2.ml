module IntSet = Set.Make (Int)

let split_into_pair s =
  match Str.split (Str.regexp "-") s with
  | a :: b :: _ -> (String.trim a, String.trim b)
  | _ ->
      Printf.eprintf "Invalid range format %s\n" s;
      exit 1

let rec pow10 i = match i with 1 -> 1 | _ -> 10 * pow10 (i - 1)
(*$=
  1     (pow10 1)
  10    (pow10 2)
  100   (pow10 3)
*)

let range ?(from = 0) ?(step = 1) until =
  Seq.ints 0
  |> Seq.map (fun i -> from + (i * step))
  |> Seq.take_while (fun i -> i <= until)

(* Creates a sequence f..t inclusive *)
let ( -- ) f t = range ~from:f t
let rec repeat s n = match n with 0 -> "" | _ -> s ^ repeat s (n - 1)
(*$=
  ""            (repeat "hi" 0)
  "hi"          (repeat "hi" 1)
  "hihi"        (repeat "hi" 2)
  "hihihihihi"  (repeat "hi" 5)
*)

let find_invalid_numbers part (first, last) =
  let first_i = int_of_string first in
  let last_i = int_of_string last in
  String.length first -- String.length last
  |> Seq.flat_map (fun len ->
      1 -- (len / 2)
      |> Seq.filter (fun part_len -> len mod part_len = 0)
      |> Seq.filter (fun part_len -> part = 2 || len / part_len = 2)
      |> Seq.flat_map (fun part_len ->
          let start = pow10 part_len in
          Seq.ints start
          |> Seq.map (fun i ->
              let si = string_of_int i in
              int_of_string (repeat si (len / part_len)))
          |> Seq.drop_while (fun i -> i < first_i)
          |> Seq.take_while (fun i -> i <= last_i)))

let sum xs = Seq.fold_left ( + ) 0 xs
let unique seq = seq |> IntSet.of_seq |> IntSet.to_seq

let solve_part part input =
  input
  |> Str.split (Str.regexp ",")
  |> List.map split_into_pair |> List.to_seq
  |> Seq.flat_map (find_invalid_numbers part)
  |> unique |> sum

let solve input =
  let part1 = solve_part 1 input in
  let part2 = solve_part 2 input in
  Printf.printf "Part 1: %d  Part 2: %d\n" part1 part2
