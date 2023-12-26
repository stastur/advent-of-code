open Base

let parse_edges line =
  match String.split ~on:' ' line with
  | parent :: children ->
      List.map children ~f:(fun child -> (String.drop_suffix parent 1, child))
  | _ -> failwith "invalid input"

let get_vertices edges =
  List.concat_map edges ~f:(fun (parent, child) -> [ parent; child ])
  |> Set.of_list (module String)

let karger vertices edges =
  let rm = Fn.flip Set.remove in
  let add = Fn.flip Set.add in

  let rec aux nodes edges =
    if Set.length nodes <= 2 then (Set.to_list nodes, edges)
    else
      let open String in
      let u, v = List.random_element_exn edges in
      let merged = Printf.sprintf "%s:%s" u v in

      let nodes = nodes |> rm u |> rm v |> add merged in

      let edges =
        List.map edges ~f:(fun (l, r) ->
            let left = if l = u || l = v then merged else l in
            let right = if r = u || r = v then merged else r in
            (left, right))
        |> List.filter ~f:(fun (l, r) -> l <> r)
      in

      aux nodes edges
  in
  aux vertices edges

let () =
  let edges = Utils.read_lines () |> List.concat_map ~f:parse_edges in
  let vertices = get_vertices edges in

  let part1 =
    let count_nodes s = String.count s ~f:(Char.equal ':') + 1 in

    let rec aux () =
      match karger vertices edges with
      | [ a; b ], edges when List.length edges = 3 ->
          count_nodes a * count_nodes b
      | _ -> aux ()
    in
    aux ()
  in

  Utils.print_result (Int.to_string part1) ""
