open Base

let directions = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

let get_connections dir =
  match dir with
  (* up *)
  | 0, -1 -> ( function '|' | 'J' | 'L' | 'S' -> [ '|'; 'F'; '7' ] | _ -> [])
  (* right *)
  | 1, 0 -> ( function '-' | 'F' | 'L' | 'S' -> [ 'J'; '-'; '7' ] | _ -> [])
  (* down *)
  | 0, 1 -> ( function '|' | '7' | 'F' | 'S' -> [ '|'; 'L'; 'J' ] | _ -> [])
  (* left *)
  | -1, 0 -> ( function '-' | 'J' | '7' | 'S' -> [ '-'; 'F'; 'L' ] | _ -> [])
  | _ -> fun _ -> []

let get maze x y =
  let open Option in
  List.nth maze y >>= fun line -> List.nth line x

let walk_maze start_pos maze =
  let rec aux (prev_x, prev_y) (x, y) path =
    match get maze x y with
    | None -> None
    | Some 'S' when List.length path > 0 -> Some path
    | Some curr ->
        let walkable_dirs =
          List.filter directions ~f:(fun (dx, dy) ->
              match get maze (x + dx) (y + dy) with
              | None -> false
              | Some 'S' -> true
              | Some next ->
                  List.mem
                    (get_connections (dx, dy) curr)
                    next ~equal:Char.equal)
        in
        List.filter_map walkable_dirs ~f:(fun (dx, dy) ->
            let next_x, next_y = (x + dx, y + dy) in
            if next_x = prev_x && next_y = prev_y then None
            else aux (x, y) (next_x, next_y) ((next_x, next_y) :: path))
        |> List.max_elt ~compare:(fun a b ->
               Int.compare (List.length a) (List.length b))
  in

  aux start_pos start_pos []

let count_intersections line =
  let rec aux rest open_corner count =
    match rest with
    | [] -> count
    | ch :: tl -> (
        match open_corner with
        | Some 'F' -> (
            match ch with
            | 'J' -> aux tl None (count + 1)
            | '-' -> aux tl open_corner count
            | _ -> aux tl None count)
        | Some 'L' -> (
            match ch with
            | '7' -> aux tl None (count + 1)
            | '-' -> aux tl open_corner count
            | _ -> aux tl None count)
        | Some _ | None -> (
            match ch with
            | 'F' -> aux tl (Some 'F') count
            | 'L' -> aux tl (Some 'L') count
            | '|' -> aux tl open_corner (count + 1)
            | _ -> aux tl open_corner count))
  in
  aux line None 0

let equal_point (x1, y1) (x2, y2) = x1 = x2 && y1 = y2

let unify_tiles maze loop =
  List.mapi maze ~f:(fun y line ->
      List.mapi line ~f:(fun x ch ->
          let is_part_of_loop = List.exists loop ~f:(equal_point (x, y)) in
          if is_part_of_loop then ch else '.'))

let find_2d maze q =
  List.findi_exn maze ~f:(fun _ line -> List.exists line ~f:(Char.equal 'S'))
  |> fun (y, line) ->
  let x = List.findi_exn line ~f:(fun _ ch -> Char.equal ch q) |> fst in
  (x, y)

let pp ?(stars = []) maze =
  List.iteri maze ~f:(fun y line ->
      List.iteri line ~f:(fun x ch ->
          Stdlib.print_string
            (match ch with
            | 'F' -> "╭"
            | 'L' -> "╰"
            | '7' -> "╮"
            | 'J' -> "╯"
            | ch ->
                if List.mem stars (x, y) ~equal:equal_point then "*"
                else String.of_char ch));
      Stdlib.print_newline ())

let () =
  let lines = Utils.read_lines () in
  let maze = List.map lines ~f:(fun line -> String.to_list line) in
  let start_pos = find_2d maze 'S' in
  let loop = walk_maze start_pos maze |> Option.value_exn in
  let maze = unify_tiles maze loop in

  let part1 = List.length loop / 2 in
  let part2 =
    let enclosed =
      List.concat_mapi maze ~f:(fun y ->
          List.filter_mapi ~f:(fun x _ ->
              let is_part_of_loop = List.exists loop ~f:(equal_point (x, y)) in
              if is_part_of_loop then None
              else
                let line = List.drop (List.nth_exn maze y) x in
                if count_intersections line % 2 = 1 then Some (x, y) else None))
    in

    pp ~stars:enclosed maze;

    List.length enclosed
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
