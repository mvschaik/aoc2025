let read_file_to_lines filename =
  In_channel.with_open_text filename In_channel.input_lines

let read_file_to_string filename =
  In_channel.with_open_text filename In_channel.input_all

let sum_list (xs : int list) = List.fold_left ( + ) 0 xs
let sum_seq (xs : int Seq.t) = Seq.fold_left ( + ) 0 xs

let parse_map input : (int * int, char) Hashtbl.t =
  Str.split (Str.regexp "\n") input
  |> List.to_seq
  |> Seq.mapi (fun row line ->
      String.to_seq line |> Seq.mapi (fun col c -> ((row, col), c)))
  |> Seq.concat |> Hashtbl.of_seq

let dims map =
  Hashtbl.fold
    (fun (row, col) _ (min_row, max_row, min_col, max_col) ->
      (min row min_row, max row max_row, min col min_col, max col max_col))
    map
    (max_int, min_int, max_int, min_int)

(*$T
  (try ignore (read_file_to_lines "dune-project"); true with _ -> true)
*)
