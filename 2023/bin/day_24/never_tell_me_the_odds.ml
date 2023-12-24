open Base
open Float

type point_3d = { x : float; y : float; z : float }
[@@deriving sexp, compare, equal]

type t = point_3d * point_3d [@@deriving sexp, compare, equal]

let print t =
  let { x; y; z }, { x = vx; y = vy; z = vz } = t in
  Stdio.printf "%f, %f, %f, @ %f, %f, %f\n" x y z vx vy vz

let parse_line line =
  String.split_on_chars line ~on:[ '@'; ',' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:String.strip
  |> List.map ~f:Float.of_string
  |> function
  | [ x; y; z; vx; vy; vz ] -> ({ x; y; z }, { x = vx; y = vy; z = vz })
  | _ -> failwith "invalid input"

let det a b c d = (a * d) - (b * c)

let cramers_rule a1 b1 s1 a2 b2 s2 =
  let d = det a1 b1 a2 b2 in
  let dx = det s1 b1 s2 b2 in
  let dy = det a1 s1 a2 s2 in

  if d = 0. then None else Some (dx /. d, dy /. d)

let find_intersection_point_2d (p1, v1) (p2, v2) =
  match cramers_rule v1.x (-v2.x) (p2.x - p1.x) v1.y (-v2.y) (p2.y - p1.y) with
  | None -> None
  | Some (t, s) ->
      if t < 0. || s < 0. then None
      else Some { x = p1.x +. (t *. v1.x); y = p1.y +. (t *. v1.y); z = 0. }

let build_pairs l =
  let rec aux l acc =
    match l with
    | [] -> acc
    | hd :: tl ->
        let pairs = List.map ~f:(fun x -> (hd, x)) tl in
        aux tl (acc @ pairs)
  in
  aux l []

let proportions a b = (a.x /. b.x, a.y /. b.y, a.z /. b.z)

let distance a b =
  let x = b.x -. a.x in
  let y = b.y -. a.y in
  let z = b.z -. a.z in
  sqrt ((x *. x) +. (y *. y) +. (z *. z))

let are_on_same_line a b c =
  let abx = b.x -. a.x in
  let aby = b.y -. a.y in
  let abz = b.z -. a.z in
  let bcx = c.x -. b.x in
  let bcy = c.y -. b.y in
  let bcz = c.z -. b.z in

  abx /. bcx = aby /. bcy && abx /. bcx = abz /. bcz

let all_points_on_same_line points =
  let rec aux points =
    match points with
    | a :: b :: c :: tl -> are_on_same_line a b c && aux (b :: c :: tl)
    | _ -> true
  in
  aux points

let find_pos_at t ~h:(p, v) =
  let x = p.x +. (t *. v.x) in
  let y = p.y +. (t *. v.y) in
  let z = p.z +. (t *. v.z) in
  { x; y; z }

let solve_1 h0 h1 h2 =
  try
    Some
      (for t = 1 to 1000 do
         let p0 = find_pos_at (Int.to_float t) ~h:h0 in

         for t = 1 to 1000 do
           let p1 = find_pos_at (Int.to_float t) ~h:h1 in

           for t = 1 to 1000 do
             let p2 = find_pos_at (Int.to_float t) ~h:h2 in

             if are_on_same_line p0 p1 p2 then (
               Stdio.printf "%f %f %f\n" p0.x p0.y p0.z;
               Stdio.printf "%f %f %f\n" p1.x p1.y p1.z;
               Stdio.printf "%f %f %f\n" p2.x p2.y p2.z;
               failwith "found")
           done
         done
       done)
  with _ -> None

let () =
  let hails = Utils.read_lines () |> List.map ~f:parse_line in

  (* let min = 200000000000000. in
     let max = 400000000000000. in *)
  let min = 7. in
  let max = 27. in

  let part1 =
    let pairs = build_pairs hails in
    let intersections =
      List.filter_map pairs ~f:(fun (a, b) -> find_intersection_point_2d a b)
      |> List.filter ~f:(fun { x; y; _ } ->
             x >= min && x <= max && y >= min && y <= max)
    in
    List.length intersections
  in

  (* let rec loop hs =
       match hs with
       | a :: b :: c :: tl ->
           solve_1 a b c |> ignore;
           loop (b :: c :: tl)
       | _ -> None
     in

     loop hails |> ignore;

     let _ =
       match hails with
       | h0 :: h1 :: h2 :: _ -> solve_1 h0 h1 h2
       | _ -> failwith "invalid input"
     in *)
  Utils.print_result (Int.to_string part1) ""
