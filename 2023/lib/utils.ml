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

let lcm a b =
  let rec gcd a b = if a = 0 then b else gcd (b mod a) a in
  a * b / gcd a b

let lcm_of_list = function
  | hd :: tl -> List.fold_left lcm hd tl
  | [] -> failwith "Not enough elements in list"

let print_result p1 p2 =
  Printf.printf "Part 1: %s\n" p1;
  Printf.printf "Part 2: %s\n" p2

let to_int_list ~sep str =
  str |> String.split_on_char sep |> List.map int_of_string
