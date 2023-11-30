let calculate_cals lines =
  let add_to_head list value =
    match list with [] -> [ value ] | acc :: rest -> (value + acc) :: rest
  in

  List.fold_left
    (fun groups line ->
      match line with
      | "" -> 0 :: groups
      | cals -> add_to_head groups (int_of_string cals))
    [] lines

let max_of_list input = List.fold_left max 0 input

let max_3 input =
  match List.sort compare input |> List.rev with
  | a :: b :: c :: _ -> [ a; b; c ]
  | _ -> []

let () =
  let lines = Utils.read_lines () in
  let groups = calculate_cals lines in

  Utils.print_result
    (max_of_list groups |> string_of_int)
    (List.fold_left ( + ) 0 (max_3 groups) |> string_of_int)
