open Base

let count_free_spots sorted_nums =
  let rec aux nums acc =
    match nums with
    | hd :: next :: tl -> aux (next :: tl) (acc + (next - hd - 1))
    | _ -> acc
  in
  aux sorted_nums 0

let expand factor coords =
  let sorted_xs, sorted_ys =
    List.unzip coords |> fun (xs, ys) ->
    ( List.dedup_and_sort ~compare:Int.compare xs,
      List.dedup_and_sort ~compare:Int.compare ys )
  in

  let count_preceding_expansions nums i =
    List.take_while nums ~f:(Fn.compose not (Int.equal i)) @ [ i ]
    |> count_free_spots
  in

  List.map coords ~f:(fun (x, y) ->
      let x_exp = count_preceding_expansions sorted_xs x in
      let y_exp = count_preceding_expansions sorted_ys y in
      (x - x_exp + (x_exp * factor), y - y_exp + (y_exp * factor)))

let get_galaxies =
  List.foldi ~init:[] ~f:(fun y acc ->
      List.foldi ~init:acc ~f:(fun x acc cell ->
          match cell with '#' -> (x, y) :: acc | _ -> acc))

let distance (ax, ay) (bx, by) = Int.abs (ax - bx) + Int.abs (ay - by)

let get_pairs list =
  let rec aux list =
    match list with
    | [] -> []
    | hd :: tl -> List.map ~f:(fun x -> (hd, x)) tl @ aux tl
  in
  aux list

let () =
  let lines = Utils.read_lines () in
  let grid = List.map lines ~f:(fun line -> String.to_list line) in

  let solve factor =
    let pairs = grid |> get_galaxies |> expand factor |> get_pairs in
    let distances = List.map pairs ~f:(fun (a, b) -> distance a b) in
    List.sum (module Int) distances ~f:Fn.id |> Int.to_string
  in

  Utils.print_result (solve 2) (solve 1_000_000)
