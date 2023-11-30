open Base

let make_rope hd l = List.init l ~f:(fun _ -> hd)

let update_rope rope hd =
  let rope = hd :: List.tl_exn rope in

  let rec aux rope =
    match rope with
    | first :: second :: tl ->
        let dx = abs (fst first - fst second) in
        let dy = abs (snd first - snd second) in

        if dx <= 1 && dy <= 1 then rope
        else
          let x = if dx = 1 then fst first else (fst first + fst second) / 2 in
          let y = if dy = 1 then snd first else (snd first + snd second) / 2 in
          first :: aux ((x, y) :: tl)
    | _ -> rope
  in
  aux rope

let apply_motions motions rope_length =
  let rope = ref (make_rope (0, 0) rope_length) in
  let visited = ref [ (0, 0) ] in

  List.iter motions ~f:(fun (direction, steps) ->
      for _ = 1 to steps do
        let hx, hy = List.hd_exn !rope in
        let new_hd =
          match direction with
          | "L" -> (hx - 1, hy)
          | "R" -> (hx + 1, hy)
          | "U" -> (hx, hy + 1)
          | "D" -> (hx, hy - 1)
          | _ -> failwith "unknown direction"
        in

        rope := update_rope !rope new_hd;
        visited := List.last_exn !rope :: !visited
      done);

  !visited
  |> List.dedup_and_sort ~compare:(fun (x1, y1) (x2, y2) ->
         match compare x1 x2 with 0 -> compare y1 y2 | c -> c)
  |> List.length

let parse_motion str =
  match String.split str ~on:' ' with
  | [ direction; steps ] -> (direction, Int.of_string steps)
  | _ -> failwith "invalid motion"

let () =
  let motions = Utils.read_lines () |> List.map ~f:parse_motion in
  Utils.print_result
    (Stdlib.string_of_int (apply_motions motions 2))
    (Stdlib.string_of_int (apply_motions motions 10))
