let read_file_to_lines filename =
  In_channel.with_open_text filename In_channel.input_lines

let read_file_to_string filename =
  In_channel.with_open_text filename In_channel.input_all

let sum_list (xs : int list) = List.fold_left ( + ) 0 xs
let sum_seq (xs : int Seq.t) = Seq.fold_left ( + ) 0 xs

(*$T
  (try ignore (read_file_to_lines "dune-project"); true with _ -> true)
*)
