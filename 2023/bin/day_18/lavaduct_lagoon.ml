open Base

type direction = Up | Down | Left | Right

type t = { dir : direction; steps : int; color : string }

let map_dir = function
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Up -> (0, -1)

let parse line =
  Stdlib.Scanf.sscanf line "%c %d (#%[^)])" (fun dir steps color ->
      {
        steps;
        color;
        dir =
          (match dir with
          | 'R' -> Right
          | 'L' -> Left
          | 'U' -> Up
          | 'D' -> Down
          | _ -> failwith "Invalid direction");
      })

let restore_from_color color =
  let steps = "0x" ^ String.sub color ~pos:0 ~len:5 |> Int.Hex.of_string in
  let dir =
    match "0x" ^ String.sub color ~pos:5 ~len:1 |> Int.Hex.of_string with
    | 0 -> Right
    | 1 -> Down
    | 2 -> Left
    | 3 -> Up
    | _ -> failwith "Invalid direction"
  in
  { steps; dir; color }

let get_perimeter start_pos instructions =
  let rec aux (x, y) instructions (vs, perimeter) =
    match instructions with
    | [] -> (vs, perimeter)
    | { dir; steps; _ } :: tl ->
        let dx, dy = map_dir dir in
        let next_v = (x + (dx * steps), y + (dy * steps)) in

        aux next_v tl ((x, y) :: vs, perimeter + steps)
  in
  aux start_pos instructions ([], 0)

let area points perimeter =
  let hd = List.hd_exn points in

  let rec aux list acc =
    match list with
    | (ax, ay) :: (bx, by) :: tl ->
        let d = (ax * by) - (ay * bx) in
        aux ((bx, by) :: tl) (acc + d)
    | _ -> ((Int.abs acc + perimeter) / 2) + 1
  in

  aux (points @ [ hd ]) 0

let () =
  let instructions = Utils.read_lines () |> List.map ~f:parse in

  let part1 =
    let vs, perimeter = get_perimeter (0, 0) instructions in
    area vs perimeter
  in

  let part2 =
    let instructions =
      instructions |> List.map ~f:(fun x -> restore_from_color x.color)
    in
    let vs, perimeter = get_perimeter (0, 0) instructions in
    area vs perimeter
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
