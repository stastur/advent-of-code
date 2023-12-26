open Base

type point_3d = { x : int; y : int; z : int } [@@deriving sexp, compare, equal]

type t = point_3d * point_3d [@@deriving sexp, compare, equal]

let parse_line line =
  String.split_on_chars line ~on:[ '@'; ',' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:String.strip |> List.map ~f:Int.of_string
  |> function
  | [ x; y; z; vx; vy; vz ] -> ({ x; y; z }, { x = vx; y = vy; z = vz })
  | _ -> failwith "invalid input"

let get_cofactor m row col =
  Array.filteri m ~f:(fun i _ -> i <> row)
  |> Array.transpose_exn
  |> Array.filteri ~f:(fun i _ -> i <> col)
  |> Array.transpose_exn

let rec determinant_of_matrix m =
  if Array.length m = 1 then m.(0).(0)
  else
    Array.foldi m ~init:0 ~f:(fun i d _ ->
        let sign = if i % 2 = 0 then 1 else -1 in
        let cofactor = get_cofactor m 0 i in
        d + (sign * m.(0).(i) * determinant_of_matrix cofactor))

let cramer matrix =
  let n = Array.length matrix.(0) in

  let di m idx =
    let m = Array.to_list m |> List.map ~f:Array.to_list in
    let m =
      List.map m ~f:(fun l ->
          let last = List.last_exn l in
          List.drop_last_exn l
          |> List.mapi ~f:(fun i x -> if i = idx then last else x))
    in
    m |> List.map ~f:Array.of_list |> Array.of_list
  in

  let d = Int.to_float @@ determinant_of_matrix (di matrix (n - 1)) in

  if Float.(d = 0.) then None
  else
    List.range 0 (n - 1)
    |> List.map ~f:(fun i ->
           Int.to_float (determinant_of_matrix (di matrix i)) /. d)
    |> Option.some

let find_intersection_point_2d (p1, v1) (p2, v2) =
  let a1 = v1.x in
  let b1 = -v2.x in
  let s1 = p2.x - p1.x in

  let a2 = v1.y in
  let b2 = -v2.y in
  let s2 = p2.y - p1.y in

  match cramer [| [| a1; b1; s1 |]; [| a2; b2; s2 |] |] with
  | Some [ t; s ] ->
      if Float.(t < 0. || s < 0.) then None
      else
        let x = Int.to_float p1.x in
        let y = Int.to_float p1.y in
        let vx = Int.to_float v1.x in
        let vy = Int.to_float v1.y in
        Some (x +. (t *. vx), y +. (t *. vy))
  | _ -> None

let build_pairs l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd :: tl ->
        let pairs = List.map ~f:(fun x -> (hd, x)) tl in
        aux tl (acc @ pairs)
  in
  aux l []

let make_equation (p0, v0) (p1, v1) =
  let a = v0.y - v1.y in
  let b = v0.x - v1.x in
  let c = p1.y - p0.y in
  let d = p0.x - p1.x in
  let s = (p0.x * v0.x) - (p0.y * v0.x) + (p1.y * v1.x) - (p1.x * v1.y) in
  (* x; y; vx; vy; *)
  [| a; b; c; d; s |]

let () =
  let hails = Utils.read_lines () |> List.map ~f:parse_line in

  (* let left_bound = 200000000000000. in
     let right_bound = 400000000000000. in *)
  let left_bound = 7. in
  let right_bound = 27. in

  let part1 =
    let pairs = build_pairs hails in
    let intersections =
      List.filter_map pairs ~f:(fun (a, b) -> find_intersection_point_2d a b)
      |> List.filter ~f:(fun (x, y) ->
             Float.(
               x >= left_bound && x <= right_bound && y >= left_bound
               && y <= right_bound))
    in
    List.length intersections
  in

  let _ =
    let equations =
      let rec aux acc hs =
        match hs with
        | hd :: next :: tl -> aux (make_equation hd next :: acc) tl
        | _ -> acc
      in
      List.take hails 5 |> aux [] |> Array.of_list
    in
    match cramer equations with
    | Some [ x; y; vx; vy ] ->
        Stdio.printf "x: %f, y: %f, vx: %f, vy: %f\n" x y vx vy
    | _ -> failwith "no solution"
  in
  Utils.print_result (Int.to_string part1) ""
