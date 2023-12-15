open Base

module Lense = struct
  type t = { label : string; focal_length : int; index : int }

  let compare a b = String.compare a.label b.label

  let equal a b = compare a b = 0

  let has_label label lense = String.equal label lense.label
end

module Op = struct
  type t = Remove of { label : string; index : int } | Replace of Lense.t

  let hash =
    String.fold ~init:0 ~f:(fun v ch -> (v + Char.to_int ch) * 17 % 256)

  let of_string s =
    Stdlib.Scanf.sscanf s "%[^=-]%[=-]%s" (fun label op fl_str ->
        let index = hash label in
        match op with
        | "-" -> Remove { label; index }
        | "=" -> Replace { label; focal_length = Int.of_string fl_str; index }
        | _ -> failwith "invalid step")
end

let () =
  let lines = List.hd_exn (Utils.read_lines ()) |> String.split ~on:',' in
  let part1 = List.fold lines ~init:0 ~f:(fun acc s -> acc + Op.hash s) in
  let part2 =
    let ops = List.map lines ~f:Op.of_string in
    let boxes = Array.create ~len:256 ([] : Lense.t list) in

    List.iter ops ~f:(fun op ->
        match op with
        | Op.Remove { index; label } ->
            Array.get boxes index
            |> List.filter ~f:(Fn.non (Lense.has_label label))
            |> Array.set boxes index
        | Op.Replace l ->
            let box = Array.get boxes l.index in
            let updated =
              if List.exists box ~f:(Lense.equal l) then
                List.map box ~f:(fun curr ->
                    if Lense.equal curr l then l else curr)
              else box @ [ l ]
            in
            Array.set boxes l.index updated);

    Array.fold boxes ~init:0 ~f:(fun acc lenses ->
        List.foldi lenses ~init:acc ~f:(fun i acc l ->
            acc + (l.focal_length * (i + 1) * (l.index + 1))))
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
