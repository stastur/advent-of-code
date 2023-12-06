open Base

let count_better_ways (t, d) =
  let rec aux hold count =
    if hold >= t - 1 then count
    else
      let curr_d = hold * (t - hold) in
      aux (hold + 1) (count + Bool.to_int (curr_d > d))
  in
  aux 1 0

let () =
  let lines = Utils.read_lines () in
  let data =
    let ts, ds =
      let to_list_of_ints line =
        String.split ~on:':' line |> List.last_exn |> String.split ~on:' '
        |> List.filter_map ~f:Stdlib.int_of_string_opt
      in
      match lines with
      | [ t; d ] -> (to_list_of_ints t, to_list_of_ints d)
      | _ -> failwith "invalid input"
    in
    List.zip_exn ts ds
  in

  let part1 =
    List.fold data ~init:1 ~f:(fun acc race -> acc * count_better_ways race)
  in
  let part2 =
    (match lines with
    | [ t; d ] ->
        let to_int line =
          String.split ~on:':' line |> List.last_exn
          |> String.filter ~f:Char.is_digit
          |> Int.of_string
        in
        (to_int t, to_int d)
    | _ -> failwith "invalid input")
    |> count_better_ways
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
