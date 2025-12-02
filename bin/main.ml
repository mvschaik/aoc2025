let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.eprintf "Usage: %s <day number> <filename>\n" Sys.argv.(0);
    exit 1
  end;
  match int_of_string Sys.argv.(1) with
  | 1 ->
      let lines = Aoc.General.read_file_to_lines Sys.argv.(2) in
      Aoc.Day1.solve lines
  | 2 -> Aoc.Day2.solve (Aoc.General.read_file_to_string Sys.argv.(2))
  | _ ->
      Printf.eprintf "Invalid day %s\n" Sys.argv.(1);
      exit 1
