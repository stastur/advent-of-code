open Base
open Pairing_heap
module Heap = Pairing_heap

type direction = Up | Down | Left | Right
[@@deriving compare, equal, sexp_of, hash]

type pos = { x : int; y : int } [@@deriving compare, equal, sexp_of, hash]

let directions = [ Up; Down; Left; Right ]

module T = struct
  type t = { pos : pos; dir : direction; steps : int }
  [@@deriving compare, equal, sexp_of, hash]
end

let opposite_of = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left

let map_dir = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let get grid { x; y } =
  List.nth_exn (List.nth_exn grid y) x |> Char.get_digit_exn

let find_paths grid start_pos end_pos min max =
  let w = List.length (List.nth_exn grid 0) in
  let h = List.length grid in

  let cost_so_far = Hashtbl.create (module T) in

  let frontier =
    Heap.create ~cmp:(fun (_, cost_a) (_, cost_b) -> cost_a - cost_b) ()
  in

  let setup t =
    Heap.add frontier (t, 0);
    Hashtbl.set cost_so_far ~key:t ~data:0
  in

  setup { T.pos = start_pos; dir = Right; steps = 0 };
  setup { T.pos = start_pos; dir = Down; steps = 0 };

  let costs = ref [] in

  while not (Heap.is_empty frontier) do
    let curr, _ = Heap.pop_exn frontier in

    if equal_pos curr.pos end_pos then
      costs := Hashtbl.find_exn cost_so_far curr :: !costs
    else
      let neighbours =
        directions
        |> List.filter ~f:(fun dir ->
               not (equal_direction dir (opposite_of curr.dir)))
        |> List.map ~f:(fun dir ->
               let dx, dy = map_dir dir in
               let pos = { x = curr.pos.x + dx; y = curr.pos.y + dy } in
               let is_same_dir = equal_direction dir curr.dir in

               {
                 T.pos;
                 dir;
                 steps = (if is_same_dir then curr.steps + 1 else 0);
               })
        |> List.filter ~f:(fun { pos; _ } ->
               pos.x >= 0 && pos.x < w && pos.y >= 0 && pos.y < h)
        |> List.filter ~f:(fun { steps; dir; _ } ->
               let is_same_dir = equal_direction dir curr.dir in

               if curr.steps < min then is_same_dir
               else if steps >= max then not is_same_dir
               else true)
      in

      List.iter neighbours ~f:(fun n ->
          let new_cost = Hashtbl.find_exn cost_so_far curr + get grid n.pos in

          let should_visit =
            (not (Hashtbl.mem cost_so_far n))
            || new_cost < Hashtbl.find_exn cost_so_far n
          in

          if should_visit then (
            Hashtbl.set cost_so_far ~key:n ~data:new_cost;
            Heap.add frontier (n, new_cost)));
      ()
  done;

  Hashtbl.filter_keys cost_so_far ~f:(fun k ->
      equal_pos k.pos end_pos && k.steps >= min)
  |> Hashtbl.to_alist |> List.map ~f:snd

let () =
  let grid = Utils.read_lines () |> List.map ~f:String.to_list in
  let w = List.length (List.nth_exn grid 0) in
  let h = List.length grid in

  let solve min max =
    find_paths grid { x = 0; y = 0 } { x = w - 1; y = h - 1 } min max
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in

  let part1 = solve 0 3 in
  let part2 = solve 3 10 in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
