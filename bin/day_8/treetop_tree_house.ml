open Base

let map_grid grid ~map_direction ~fold_directions =
  Array.mapi grid ~f:(fun y row ->
      Array.mapi
        ~f:(fun x tree ->
          List.map
            ~f:(fun (dx, dy) ->
              map_direction grid tree (x + dx) (y + dy) (dx, dy))
            [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
          |> fold_directions)
        row)

let is_in_bounds grid x y =
  let rows = Array.length grid and cols = Array.length grid.(0) in
  0 <= x && x < cols && 0 <= y && y < rows

let rec is_visible grid tree x y (dx, dy) =
  match (x, y) with
  (* no cover *)
  | _, _ when not (is_in_bounds grid x y) -> true
  (* has cover at (x, y) *)
  | _, _ when tree <= grid.(y).(x) -> false
  (* check next position *)
  | _, _ -> is_visible grid tree (x + dx) (y + dy) (dx, dy)

let rec score grid tree x y (dx, dy) =
  match (x, y) with
  (* tree is on the edge *)
  | _, _ when not (is_in_bounds grid x y) -> 0
  (* view is blocked *)
  | _, _ when tree <= grid.(y).(x) -> 1
  (* check next tree *)
  | _, _ -> 1 + score grid tree (x + dx) (y + dy) (dx, dy)

let () =
  let lines = Utils.read_lines () in
  let grid =
    Array.of_list_map
      ~f:(fun line -> String.to_array line |> Array.map ~f:Char.to_int)
      lines
  in

  let part1 =
    map_grid grid ~map_direction:is_visible
      ~fold_directions:(List.fold_left ~f:( || ) ~init:false)
    |> Array.fold ~f:(fun acc row -> acc + Array.count ~f:Fn.id row) ~init:0
    |> Int.to_string
  and part2 =
    map_grid grid ~map_direction:score
      ~fold_directions:(List.fold_left ~f:( * ) ~init:1)
    |> Array.fold
         ~f:(fun acc row -> max acc (Array.fold ~f:max ~init:0 row))
         ~init:0
    |> Int.to_string
  in
  Utils.print_result part1 part2
