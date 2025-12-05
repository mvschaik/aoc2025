let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.eprintf "Usage: %s <day number> <filename>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(2) in
  match int_of_string Sys.argv.(1) with
  | 1 -> General.read_file_to_lines filename |> Day1.solve
  | 2 -> General.read_file_to_string filename |> Day2.solve
  | 3 -> General.read_file_to_lines filename |> Day3.solve
  | 4 -> General.read_file_to_string filename |> Day4.solve
  | _ ->
      Printf.eprintf "Invalid day %s\n" Sys.argv.(1);
      exit 1
