type range = { left : int; right : int }

let get_range_pairs s =
  Scanf.sscanf s "%d-%d,%d-%d" (fun l1 r1 l2 r2 ->
      ({ left = l1; right = r1 }, { left = l2; right = r2 }))

let is_in_range r n = r.left <= n && n <= r.right

let contains (r1, r2) = is_in_range r1 r2.left && is_in_range r1 r2.right

let overlaps (r1, r2) =
  is_in_range r1 r2.left || is_in_range r1 r2.right || is_in_range r2 r1.left
  || is_in_range r2 r1.right

let count f lines =
  lines |> List.map get_range_pairs |> List.filter f |> List.length

let part1 lines =
  count (fun (r1, r2) -> contains (r1, r2) || contains (r2, r1)) lines

let part2 lines = count (fun (r1, r2) -> overlaps (r1, r2)) lines

let () =
  let lines = Utils.read_lines () in
  Utils.print_result
    (part1 lines |> string_of_int)
    (part2 lines |> string_of_int)
