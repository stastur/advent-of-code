open Base

let is_unique list =
  let set = Set.of_list (module Char) list in
  List.length list = Set.length set

let find_packet_start marker_size char_list =
  let rec loop char_list idx =
    match char_list with
    | [] -> None
    | hd :: tl ->
        if is_unique (hd :: List.take tl (marker_size - 1)) then Some idx
        else loop tl (idx + 1)
  in
  loop char_list marker_size

let () =
  let signal = Utils.read_lines () |> List.hd_exn |> String.to_list in
  let part1 = find_packet_start 4 signal in
  let part2 = find_packet_start 14 signal in

  match (part1, part2) with
  | Some part1, Some part2 ->
      Utils.print_result
        (Stdlib.string_of_int part1)
        (Stdlib.string_of_int part2)
  | _ -> failwith "Invalid input"
