let rec divMod a b =
  if a < 0 then
    let r, q = divMod (a + b) b in
    (r, q + 1)
  else if a < b then (a, 0)
  else
    let r, q = divMod (a - b) b in
    (r, q + 1)

(*$= divMod & ~printer:tuple_to_string
  (0, 5)   (divMod 10 2)
  (5, 0)   (divMod 5 10)
  (1, 2)   (divMod (-3) 2)
  (0, 1)   (divMod (-2) 2)
*)

let parse_line s =
  Scanf.sscanf s "%c%d" (fun l n ->
      match l with
      | 'R' -> n
      | 'L' -> -n
      | _ ->
          Printf.eprintf "Invalid direction %c" l;
          exit 1)

let solve lines =
  let movements = List.map parse_line lines in

  let pos = ref 50 in
  let count1 = ref 0 in
  let count2 = ref 0 in

  List.iter
    (fun movement ->
      let m, q = divMod (!pos + movement) 100 in
      if !pos = 0 && movement < 0 then decr count2;
      pos := m;
      if !pos = 0 && movement > 0 then decr count2;
      count2 := !count2 + q;
      if !pos = 0 then incr count1)
    movements;
  Printf.printf "Part 1: %d  Part 2: %d\n" !count1 (!count2 + !count1)

let tuple_to_string (a, b) = Printf.sprintf "(%d, %d)" a b
