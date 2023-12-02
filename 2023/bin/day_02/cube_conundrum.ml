open Base

module GameSet = struct
  type t = { r : int; g : int; b : int }

  let default = { r = 0; g = 0; b = 0 }

  let get_spec sets =
    List.fold sets ~init:default ~f:(fun acc set ->
        { r = max acc.r set.r; g = max acc.g set.g; b = max acc.b set.b })

  let satisfies_spec set ~spec =
    set.r <= spec.r && set.g <= spec.g && set.b <= spec.b

  let from_string line =
    let items = String.split line ~on:',' in
    List.fold items ~init:default ~f:(fun acc item ->
        let n, color =
          item |> Stdlib.String.trim |> fun s ->
          Stdlib.Scanf.sscanf s "%d %s" (fun n color -> (n, color))
        in

        match color with
        | "red" -> { acc with r = n }
        | "green" -> { acc with g = n }
        | "blue" -> { acc with b = n }
        | _ -> acc)

  let power { r; g; b } = r * g * b
end

let parse_line line =
  let set_strings =
    line |> String.split ~on:':' |> List.last_exn |> String.split ~on:';'
  in
  List.map set_strings ~f:GameSet.from_string

let () =
  let games = Utils.read_lines () |> List.map ~f:parse_line in

  let part1 =
    games
    |> List.foldi ~init:0 ~f:(fun i acc sets ->
           let is_valid =
             List.for_all sets
               ~f:(GameSet.satisfies_spec ~spec:{ r = 12; g = 13; b = 14 })
           in
           if is_valid then acc + i + 1 else acc)
  in

  let part2 =
    games
    |> List.sum (module Int) ~f:(Fn.compose GameSet.power GameSet.get_spec)
  in

  Utils.print_result (Stdlib.string_of_int part1) (Stdlib.string_of_int part2)
