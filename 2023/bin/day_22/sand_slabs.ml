open Base

module Brick = struct
  type point_3d = { x : int; y : int; z : int }
  [@@deriving sexp, equal, compare, hash]

  type t = { id : int; high : point_3d; low : point_3d }
  [@@deriving sexp, equal, hash]

  let compare a b = Int.compare a.low.z b.low.z

  let of_string id s =
    let point_of_string s =
      s |> Utils.to_int_list ~sep:',' |> function
      | [ x; y; z ] -> { x; y; z }
      | _ -> failwith "Wrong format"
    in

    match String.split s ~on:'~' |> List.map ~f:point_of_string with
    | [ p1; p2 ] ->
        let high, low = if p1.z > p2.z then (p1, p2) else (p2, p1) in
        { id; high; low }
    | _ -> failwith "Wrong format"

  let intersect a b =
    a.low.x <= b.high.x && a.high.x >= b.low.x && a.low.y <= b.high.y
    && a.high.y >= b.low.y && a.low.z <= b.high.z && a.high.z >= b.low.z

  let lift ~by b =
    {
      b with
      high = { b.high with z = b.high.z + by };
      low = { b.low with z = b.low.z + by };
    }

  let drop_bricks bs =
    let rec aux left fallen =
      match left with
      | b :: bs ->
          if List.exists fallen ~f:(intersect b) || b.low.z < 1 then
            aux bs (lift ~by:1 b :: fallen)
          else aux (lift ~by:(-1) b :: bs) fallen
      | [] -> fallen
    in
    List.rev (aux bs [])

  let count_shifted ~init = List.count ~f:(Fn.non (List.mem init ~equal))
end

let () =
  let bricks =
    Utils.read_lines ()
    |> List.mapi ~f:Brick.of_string
    |> List.sort ~compare:Brick.compare
    |> Brick.drop_bricks
  in

  let shifts =
    List.map bricks ~f:(fun b ->
        let without_current = List.filter bricks ~f:(Fn.non (Brick.equal b)) in
        let dropped = Brick.drop_bricks without_current in

        Brick.count_shifted ~init:without_current dropped)
  in

  let part1 = List.count shifts ~f:(fun x -> x = 0) in
  let part2 = List.sum (module Int) shifts ~f:Fn.id in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
