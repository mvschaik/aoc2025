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

let split_in_half s =
  let split = (String.length s + 1) / 2 in
  if String.length s < 2 * split then
    let lower = pow10 split in
    (lower, lower)
  else
    ( int_of_string (Str.first_chars s split),
      int_of_string (Str.last_chars s split) )

let find_in_range (first, last) =
  let first_i = int_of_string first in
  let last_i = int_of_string last in
  let a, b = split_in_half first in
  let start = min a b in
  Seq.ints start
  |> Seq.map (fun i ->
      let si = string_of_int i in
      int_of_string (si ^ si))
  |> Seq.drop_while (fun i -> i < first_i)
  |> Seq.take_while (fun i -> i <= last_i)

let solve input =
  let part1 =
    input
    |> Str.split (Str.regexp ",")
    |> List.map split_into_pair |> List.to_seq |> Seq.flat_map find_in_range
    |> Seq.fold_left (fun a x -> a + x) 0
  in
  Printf.printf "Part 1: %d\n" part1
