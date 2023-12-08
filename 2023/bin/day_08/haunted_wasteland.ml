open Base
module Hashtbl = Stdlib.Hashtbl.Make (String)

type node = { value : string; left : string; right : string }

let node_of_string s =
  Stdlib.Scanf.sscanf s "%s = (%[^,], %[^)])" (fun value left right ->
      { value; left; right })

let walk table path dest_suffix =
  let rec aux left_path steps curr_node =
    match left_path with
    | [] when String.is_suffix curr_node.value ~suffix:dest_suffix -> steps
    | [] -> aux path steps curr_node
    | dir :: tl ->
        let next =
          match dir with
          | 'L' -> curr_node.left
          | 'R' -> curr_node.right
          | _ -> failwith "Invalid direction"
        in
        aux tl (steps + 1) (Hashtbl.find table next)
  in
  aux path 0

let lcm a b =
  let rec gcd a b = if a = 0 then b else gcd (b % a) a in
  a * b / gcd a b

let () =
  let lines = Utils.read_lines () in
  let path, nodes =
    match lines with
    | hd :: _ :: tl -> (String.to_list hd, List.map tl ~f:node_of_string)
    | _ -> failwith "Invalid input"
  in
  let graph =
    List.fold nodes ~init:(Hashtbl.create 10) ~f:(fun map node ->
        Hashtbl.add map node.value node;
        map)
  in

  let part1 = walk graph path "ZZZ" (Hashtbl.find graph "AAA") in

  let part2 =
    List.filter nodes ~f:(fun node -> String.is_suffix node.value ~suffix:"A")
    |> List.map ~f:(walk graph path "Z")
    |> function
    | hd :: tl -> List.fold tl ~init:hd ~f:lcm
    | [] -> failwith "No start nodes"
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
