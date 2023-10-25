open Base

module HeightMap = struct
  let at t (x, y) =
    Option.(List.nth t y >>= fun row -> List.nth row x >>| Char.to_int)

  let is_reachable t from pos =
    match (at t from, at t pos) with
    | Some h1, Some h2 -> h2 - h1 <= 1
    | _ -> false

  let directions = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

  let neighbours t pos =
    let x, y = pos in
    List.map ~f:(fun (dx, dy) -> (x + dx, y + dy)) directions
    |> List.filter ~f:(Fn.compose Option.is_some (at t))

  let from_lines lines =
    let a_positions = ref [] in
    let s_pos = ref (0, 0) in
    let e_pos = ref (0, 0) in
    let height_map =
      List.mapi
        ~f:(fun y row ->
          String.to_list row
          |> List.mapi ~f:(fun x ch ->
                 match ch with
                 | 'S' ->
                     s_pos := (x, y);
                     'a'
                 | 'a' ->
                     a_positions := (x, y) :: !a_positions;
                     'a'
                 | 'E' ->
                     e_pos := (x, y);
                     'z'
                 | ch -> ch))
        lines
    in

    (height_map, !s_pos, !e_pos, !a_positions)
end

let create_distance_map height_map =
  let dimx = height_map |> List.length in
  let dimy = height_map |> List.hd_exn |> List.length in

  Array.make_matrix ~dimx ~dimy Int.max_value

let set_distances distances heights s_pos =
  let rec aux (x, y) distance =
    distances.(y).(x) <- distance;
    let neighbours = HeightMap.neighbours heights (x, y) in

    List.iter
      ~f:(fun (nx, ny) ->
        if
          HeightMap.is_reachable heights (x, y) (nx, ny)
          && distance + 1 < distances.(ny).(nx)
        then aux (nx, ny) (distance + 1))
      neighbours
  in
  aux s_pos 0

let () =
  let lines = Utils.read_lines () in
  let height_map, s_pos, e_pos, a_positions = HeightMap.from_lines lines in

  let part_1 =
    let distance_map = create_distance_map height_map in
    set_distances distance_map height_map s_pos;
    distance_map.(snd e_pos).(fst e_pos)
  in

  let part_2 =
    let distance_map = create_distance_map height_map in
    List.map
      ~f:(fun s ->
        set_distances distance_map height_map s;
        distance_map.(snd e_pos).(fst e_pos))
      (s_pos :: a_positions)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  Utils.print_result (part_1 |> Int.to_string) (part_2 |> Int.to_string)
