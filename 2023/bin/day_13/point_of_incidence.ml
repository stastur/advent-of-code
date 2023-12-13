open Base

let count_diffs a b =
  List.counti a ~f:(fun i ch -> Char.(ch <> List.nth_exn b i))

let find_reflection_point smudges grid =
  let rec aux pos =
    if pos >= List.length grid then 0
    else
      let lo, hi = List.split_n grid pos in
      let diff =
        List.foldi (List.rev lo) ~init:0 ~f:(fun i total lr ->
            (match List.nth hi i with
            | Some hr -> count_diffs lr hr
            | None -> 0)
            + total)
      in

      if smudges = diff then pos else aux (pos + 1)
  in
  aux 1

let () =
  let lines = Utils.read_lines () in
  let grids =
    List.map lines ~f:String.to_list
    |> List.group ~break:(fun _ row -> List.is_empty row)
    |> List.mapi ~f:(fun i group -> if i = 0 then group else List.drop group 1)
  in

  let solve smudges =
    List.fold grids ~init:0 ~f:(fun acc grid ->
        let h = find_reflection_point smudges grid in
        let v = List.transpose_exn grid |> find_reflection_point smudges in
        acc + (100 * h) + v)
  in

  Utils.print_result (Int.to_string (solve 0)) (Int.to_string (solve 1))
