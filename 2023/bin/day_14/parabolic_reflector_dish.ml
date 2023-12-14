open Base

let tilt_left line =
  let compare a b =
    match (a, b) with
    | '#', _ -> -1
    | _, '#' -> 1
    | 'O', _ -> -1
    | _, 'O' -> 1
    | _ -> 0
  in

  line
  |> List.group ~break:(fun _ ch -> Char.(ch = '#'))
  |> List.concat_map ~f:(List.sort ~compare)

let rotate dir platform =
  match dir with
  | "left" -> platform |> List.transpose_exn |> List.rev
  | "right" -> platform |> List.transpose_exn |> List.map ~f:List.rev
  | _ -> failwith "invalid direction"

let calc_load =
  List.fold ~init:0 ~f:(fun total line ->
      List.foldi line ~init:total ~f:(fun i acc ch ->
          acc + match ch with 'O' -> List.length line - i | _ -> 0))

let equal a b = List.equal (fun a b -> List.equal Char.equal a b) a b

let () =
  let platform =
    Utils.read_lines () |> List.map ~f:String.to_list |> rotate "left"
  in

  let part1 = List.map platform ~f:tilt_left |> calc_load in

  let part2 =
    let cycle platform =
      List.range 0 4
      |> List.fold ~init:platform ~f:(fun line _ ->
             List.map line ~f:tilt_left |> rotate "right")
    in

    let rec loop n curr history =
      if n = 0 then curr
      else
        match List.findi history ~f:(fun _ -> equal curr) with
        | Some (i, _) ->
            let cycles_left = n % (i + 1) in
            loop cycles_left curr []
        | None -> loop (n - 1) (cycle curr) (curr :: history)
    in

    calc_load (loop 1_000_000_000 platform [])
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
