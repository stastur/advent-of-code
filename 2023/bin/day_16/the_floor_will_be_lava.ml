open Base

module Beam = struct
  module T = struct
    type direction = Up | Down | Left | Right
    [@@deriving compare, equal, sexp_of, hash]

    type t = { x : int; y : int; dir : direction }
    [@@deriving compare, equal, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)

  let compare_ignoring_dir a b =
    if a.x = b.x && a.y = b.y then 0 else compare a b

  let map_dir dir =
    match dir with
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

  let reflections beam grid =
    let cell = grid.(beam.y).(beam.x) in
    let dirs =
      match cell with
      | '.' -> [ beam.dir ]
      | '-' -> (
          match beam.dir with
          | Left | Right -> [ beam.dir ]
          | Up | Down -> [ Left; Right ])
      | '|' -> (
          match beam.dir with
          | Left | Right -> [ Up; Down ]
          | Up | Down -> [ beam.dir ])
      | '/' -> (
          match beam.dir with
          | Right -> [ Up ]
          | Left -> [ Down ]
          | Up -> [ Right ]
          | Down -> [ Left ])
      | '\\' -> (
          match beam.dir with
          | Right -> [ Down ]
          | Left -> [ Up ]
          | Up -> [ Left ]
          | Down -> [ Right ])
      | _ -> failwith "invalid char"
    in

    List.map dirs ~f:(fun dir ->
        let dx, dy = map_dir dir in
        { x = beam.x + dx; y = beam.y + dy; dir })
end

let cast_beam grid beam =
  let h = Array.length grid in
  let w = Array.length grid.(0) in

  let rec aux beam visited =
    if Set.mem visited beam then visited
    else
      let visited = Set.add visited beam in
      let new_beams = Beam.reflections beam grid in

      List.fold new_beams ~init:visited ~f:(fun visited beam ->
          if beam.x >= 0 && beam.x < w && beam.y >= 0 && beam.y < h then
            aux beam visited
          else visited)
  in

  aux beam (Set.empty (module Beam))
  |> Set.to_list
  |> List.dedup_and_sort ~compare:Beam.compare_ignoring_dir
  |> List.length

let edge_of grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in

  let open Beam in
  List.init w ~f:(fun x -> { x; y = 0; dir = Down })
  @ List.init w ~f:(fun x -> { x; y = h - 1; dir = Up })
  @ List.init h ~f:(fun y -> { x = 0; y; dir = Right })
  @ List.init h ~f:(fun y -> { x = w - 1; y; dir = Left })

let () =
  let grid = Utils.read_lines () |> Array.of_list_map ~f:String.to_array in

  let part1 = cast_beam grid { x = 0; y = 0; dir = Right } in
  let part2 =
    List.fold (edge_of grid) ~init:0 ~f:(fun max beam ->
        Int.max max (cast_beam grid beam))
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
