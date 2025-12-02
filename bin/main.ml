let () =
  let lines = Aoc.General.read_file_to_lines Sys.argv.(1) in
  Aoc.Day1.solve lines
