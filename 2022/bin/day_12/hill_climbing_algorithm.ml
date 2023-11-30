open Base
module Heap = Pairing_heap

type pos = { x : int; y : int }

module Pos_tbl = Stdlib.Hashtbl.Make (struct
  type t = pos

  let hash pos = Hashtbl.hash (pos.x, pos.y)

  let equal p1 p2 = p1.x = p2.x && p1.y = p2.y
end)

module HeightMap = struct
  let at t { x; y } =
    Option.(List.nth t y >>= fun row -> List.nth row x >>| Char.to_int)

  let is_reachable t from pos =
    match (at t from, at t pos) with
    | Some h1, Some h2 -> h2 - h1 <= 1
    | _ -> false

  let directions = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

  let neighbours t pos =
    List.map ~f:(fun (dx, dy) -> { x = pos.x + dx; y = pos.y + dy }) directions
    |> List.filter ~f:(Fn.compose Option.is_some (at t))

  let from_lines lines =
    let a_positions = ref [] in
    let s_pos = ref { x = 0; y = 0 } in
    let e_pos = ref { x = 0; y = 0 } in
    let height_map =
      List.mapi
        ~f:(fun y row ->
          String.to_list row
          |> List.mapi ~f:(fun x ch ->
                 match ch with
                 | 'S' ->
                     s_pos := { x; y };
                     'a'
                 | 'a' ->
                     a_positions := { x; y } :: !a_positions;
                     'a'
                 | 'E' ->
                     e_pos := { x; y };
                     'z'
                 | ch -> ch))
        lines
    in

    (height_map, !s_pos, !e_pos, !a_positions)
end

let manhattan_distance p1 p2 = Int.abs (p1.x - p2.x) + Int.abs (p1.y - p2.y)

let find_path height_map s e =
  let frontier =
    Heap.create ~cmp:(fun (_, score1) (_, score2) -> compare score1 score2) ()
  in
  let came_from = Pos_tbl.create 10 in
  let distances = Pos_tbl.create 10 in

  Heap.add frontier (s, 0);
  Pos_tbl.add distances s 0;

  let rec loop () =
    if Heap.is_empty frontier then ()
    else
      let curr, _ = Heap.pop_exn frontier in
      let dist =
        Pos_tbl.find_opt distances curr |> Option.value ~default:Int.max_value
      in

      if curr.x = e.x && curr.y = e.y then ()
      else
        HeightMap.neighbours height_map curr
        |> List.filter ~f:(HeightMap.is_reachable height_map curr)
        |> List.iter ~f:(fun n ->
               let dist_so_far = dist + 1 in
               let saved_dist =
                 Pos_tbl.find_opt distances n
                 |> Option.value_map ~default:Int.max_value ~f:Fn.id
               in

               if dist_so_far < saved_dist then (
                 let score = dist_so_far + manhattan_distance n e in
                 Heap.add frontier (n, score);
                 Pos_tbl.replace distances n dist_so_far;
                 Pos_tbl.replace came_from n curr));
      loop ()
  in
  loop ();
  Pos_tbl.find_opt distances e

let () =
  let lines = Utils.read_lines () in
  let height_map, s_pos, e_pos, a_positions = HeightMap.from_lines lines in

  let part_1 = find_path height_map s_pos e_pos |> Option.value_exn in

  let part_2 =
    List.map ~f:(fun s -> find_path height_map s e_pos) (s_pos :: a_positions)
    |> List.filter_opt
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in

  Utils.print_result (part_1 |> Int.to_string) (part_2 |> Int.to_string)
