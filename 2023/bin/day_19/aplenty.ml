open Base

type part = { x : int; m : int; a : int; s : int } [@@deriving sexp_of]

type rule = { prop : char; op : char; value : int; dest : string }
[@@deriving sexp_of]

type workflow = { name : string; rules : rule list; final_dest : string }
[@@deriving sexp_of]

let parse_workflow line =
  Stdlib.Scanf.sscanf line "%[^{]{%[^}]}" (fun name rules ->
      let segments = String.split rules ~on:',' in
      let rules = List.drop_last_exn segments in
      let final_dest = List.last_exn segments in

      let rules =
        List.map rules ~f:(fun rule ->
            Stdlib.Scanf.sscanf rule "%c%c%d:%s" (fun prop op value dest ->
                { prop; op; value; dest }))
      in
      { name; rules; final_dest })

type node_value = { prop : char; op : char; value : int } [@@deriving sexp_of]

type tree = Leaf of char | Node of node_value * tree * tree
[@@deriving sexp_of]

let parse_part line =
  Stdlib.Scanf.sscanf line "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s ->
      { x; m; a; s })

let rec eval node part =
  match node with
  | Leaf 'R' -> false
  | Leaf 'A' -> true
  | Node (v, left, right) ->
      let op =
        match v.op with
        | '>' -> ( > )
        | '<' -> ( < )
        | _ -> failwith "invalid op"
      in
      let prop =
        match v.prop with
        | 'x' -> fun p -> p.x
        | 'm' -> fun p -> p.m
        | 'a' -> fun p -> p.a
        | 's' -> fun p -> p.s
        | _ -> failwith "invalid prop"
      in
      let next_node = if op (prop part) v.value then right else left in
      eval next_node part
  | _ -> failwith "invalid node"

let build_workflow_tree workflows =
  let rec to_node wflow =
    let match_dest dest =
      match dest with
      | "R" -> Leaf 'R'
      | "A" -> Leaf 'A'
      | _ -> to_node (Hashtbl.find_exn workflows dest)
    in

    match wflow.rules with
    | [] -> match_dest wflow.final_dest
    | rule :: rules -> (
        let value = { prop = rule.prop; op = rule.op; value = rule.value } in

        let left = to_node { wflow with rules } in
        let right = match_dest rule.dest in

        match (left, right) with
        | Leaf 'R', Leaf 'R' -> Leaf 'R'
        | Leaf 'A', Leaf 'A' -> Leaf 'A'
        | _ -> Node (value, left, right))
  in

  to_node (Hashtbl.find_exn workflows "in")

type sieve = { x : int * int; m : int * int; a : int * int; s : int * int }
[@@deriving sexp_of]

let count_combinations sieve =
  let dx = snd sieve.x - fst sieve.x + 1 in
  let dm = snd sieve.m - fst sieve.m + 1 in
  let da = snd sieve.a - fst sieve.a + 1 in
  let ds = snd sieve.s - fst sieve.s + 1 in
  dx * dm * da * ds

let split sieve prop n =
  match prop with
  | 'x' ->
      ( { sieve with x = (fst sieve.x, n) },
        { sieve with x = (n + 1, snd sieve.x) } )
  | 'm' ->
      ( { sieve with m = (fst sieve.m, n) },
        { sieve with m = (n + 1, snd sieve.m) } )
  | 'a' ->
      ( { sieve with a = (fst sieve.a, n) },
        { sieve with a = (n + 1, snd sieve.a) } )
  | 's' ->
      ( { sieve with s = (fst sieve.s, n) },
        { sieve with s = (n + 1, snd sieve.s) } )
  | _ -> failwith "invalid prop"

let flip (a, b) = (b, a)

let count_passes node =
  let rec aux node sieve =
    match node with
    | Leaf 'R' -> 0
    | Leaf 'A' -> count_combinations sieve
    | Node (v, left, right) ->
        let l, r =
          match v.op with
          | '>' -> split sieve v.prop v.value
          | '<' -> flip (split sieve v.prop (v.value - 1))
          | _ -> failwith "invalid op"
        in

        aux left l + aux right r
    | _ -> failwith "invalid node"
  in

  let min = 1 in
  let max = 4000 in
  aux node { x = (min, max); m = (min, max); a = (min, max); s = (min, max) }

let () =
  let wflows, parts =
    Utils.read_lines () |> List.split_while ~f:(Fn.non String.is_empty)
  in

  let wflows =
    List.map wflows ~f:(fun l ->
        let w = parse_workflow l in
        (w.name, w))
    |> Hashtbl.of_alist_exn (module String)
  in
  let parts = List.tl_exn parts |> List.map ~f:parse_part in
  let tree = build_workflow_tree wflows in

  let part1 =
    List.filter parts ~f:(eval tree)
    |> List.fold ~init:0 ~f:(fun acc { x; m; a; s } -> acc + x + m + a + s)
  in
  let part2 = count_passes tree in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
