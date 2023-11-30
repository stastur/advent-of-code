module CharSet = Set.Make (Char)

let chunks size list =
  List.fold_left
    (fun acc item ->
      match acc with
      | hd :: tl when List.length hd < size -> (item :: hd) :: tl
      | _ -> [ item ] :: acc)
    [] list

let split_in_half str =
  let mid = String.length str / 2 in
  [ String.sub str 0 mid; String.sub str mid mid ]

let string_to_char_set s = s |> String.to_seq |> CharSet.of_seq

let find_common_char strings =
  let charsets = List.map string_to_char_set strings in
  let intersection =
    List.fold_left CharSet.inter (List.hd charsets) (List.tl charsets)
  in
  CharSet.choose_opt intersection

let get_priority item =
  1
  +
  match int_of_char item with
  | code when code >= int_of_char 'a' -> code - int_of_char 'a'
  | code when code >= int_of_char 'A' -> code - int_of_char 'A' + 26
  | _ -> 0

let lines = Utils.read_lines ()

let part_1 lines =
  List.fold_left
    (fun acc l ->
      acc
      +
      match find_common_char (split_in_half l) with
      | None -> 0
      | Some item -> get_priority item)
    0 lines

let part_2 lines =
  List.fold_left
    (fun acc group ->
      acc
      +
      match find_common_char group with
      | None -> 0
      | Some item -> get_priority item)
    0 (chunks 3 lines)

let () =
  Utils.print_result
    (lines |> part_1 |> string_of_int)
    (lines |> part_2 |> string_of_int)
