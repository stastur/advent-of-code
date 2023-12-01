open Base

let get_number_from_line line =
  let rec walk line s e =
    match (line.[s] |> Char.get_digit, line.[e] |> Char.get_digit) with
    | None, None -> walk line (s + 1) (e - 1)
    | Some _, None -> walk line s (e - 1)
    | None, Some _ -> walk line (s + 1) e
    | Some a, Some b -> (a, b)
  in
  let a, b = walk line 0 (String.length line - 1) in
  (a * 10) + b

let () =
  let lines = Utils.read_lines () in

  let part1 =
    lines |> List.map ~f:get_number_from_line |> List.fold_left ~init:0 ~f:( + )
  in

  Utils.print_result (Stdlib.string_of_int part1) ""
