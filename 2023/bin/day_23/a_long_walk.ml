open Base

let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ]

module Pos = struct
  type t = { x : int; y : int } [@@deriving sexp, compare, hash, equal]

  let move p (dx, dy) = { x = p.x + dx; y = p.y + dy }

  let is_in_bounds p ~w ~h = p.x >= 0 && p.x < w && p.y >= 0 && p.y < h
end

include Pos

let map_slide_direction = function
  | '^' -> (0, -1)
  | 'v' -> (0, 1)
  | '>' -> (1, 0)
  | '<' -> (-1, 0)
  | _ -> failwith "Invalid slide char"

let is_walkable map p =
  let h = Array.length map in
  let w = Array.length map.(0) in

  is_in_bounds p ~w ~h && Char.(map.(p.y).(p.x) <> '#')

let get_neighbours ~is_walkable p =
  List.filter_map directions ~f:(fun dir ->
      let pos = move p dir in
      if is_walkable pos then Some pos else None)

let solve_0 grid start_pos end_pos =
  let w = Array.length grid.(0) in
  let h = Array.length grid in

  let rec aux path curr =
    if List.mem path curr ~equal:Pos.equal then 0
    else if Pos.equal curr end_pos then List.length path
    else if not (is_in_bounds ~w ~h curr) then 0
    else
      match grid.(curr.y).(curr.x) with
      | '#' -> 0
      | '.' ->
          get_neighbours ~is_walkable:(is_walkable grid) curr
          |> List.map ~f:(aux (curr :: path))
          |> List.max_elt ~compare:Int.compare
          |> Option.value ~default:0
      | slide ->
          let prev = List.hd_exn path in
          let slide_dir = map_slide_direction slide in
          let is_valid_move = Pos.equal (move prev slide_dir) curr in

          if is_valid_move then aux (curr :: path) (move curr slide_dir) else 0
  in

  aux [] start_pos

let find_vertices grid =
  Array.to_list
  @@ Array.concat_mapi grid ~f:(fun y ->
         Array.filter_mapi ~f:(fun x _ ->
             let ns = get_neighbours ~is_walkable:(is_walkable grid) { x; y } in
             if List.length ns > 2 then Some { x; y } else None))

let get_edges grid vertices =
  let edges = Hashtbl.create (module Pos) in

  List.iter vertices ~f:(fun v ->
      let visited = Hashtbl.create (module Pos) in

      let frontier = Queue.create () in
      Queue.enqueue frontier (v, 0);

      while not @@ Queue.is_empty frontier do
        let curr, steps = Queue.dequeue_exn frontier in

        if (not (Pos.equal v curr)) && List.mem vertices curr ~equal:Pos.equal
        then Hashtbl.add_multi edges ~key:v ~data:(curr, steps)
        else
          let neighbours =
            get_neighbours ~is_walkable:(is_walkable grid) curr
            |> List.filter ~f:(Fn.non (Hashtbl.mem visited))
          in

          List.iter neighbours ~f:(fun n ->
              Hashtbl.set visited ~key:n ~data:();
              Queue.enqueue frontier (n, steps + 1))
      done);

  edges

let solve_1 grid start_pos end_pos =
  let vertices = start_pos :: end_pos :: find_vertices grid in
  let edges = get_edges grid vertices in

  let rec find_longest_path curr path path_length =
    if List.mem path curr ~equal:Pos.equal then 0
    else if Pos.equal curr end_pos then path_length
    else
      let adjacent_edges = Hashtbl.find_exn edges curr in

      List.map adjacent_edges ~f:(fun (next, length) ->
          find_longest_path next (curr :: path) (path_length + length))
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
  in

  find_longest_path start_pos [] 0

let () =
  let grid =
    Utils.read_lines () |> List.to_array |> Array.map ~f:String.to_array
  in
  let w = Array.length grid.(0) in
  let h = Array.length grid in

  let start = { x = 1; y = 0 } in
  let finish = { x = w - 2; y = h - 1 } in

  Utils.print_result
    (Int.to_string @@ solve_0 grid start finish)
    (Int.to_string @@ solve_1 grid start finish)
