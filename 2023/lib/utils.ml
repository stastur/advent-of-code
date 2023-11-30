let get_path_from_args () =
  let file_path = ref "" in

  let () =
    Arg.parse
      [ ("--input-path", Arg.Set_string file_path, "Path to input file") ]
      (fun s -> file_path := s)
      "day_1 [--input-path] <input_file_path>"
  in

  file_path.contents

let read_lines () =
  let ch = open_in (get_path_from_args ()) in

  let rec append_line list =
    try
      let line = input_line ch in
      append_line (line :: list)
    with End_of_file ->
      close_in ch;
      list
  in

  List.rev (append_line [])

let print_result p1 p2 =
  Printf.printf "Part 1: %s\n" p1;
  Printf.printf "Part 2: %s\n" p2
