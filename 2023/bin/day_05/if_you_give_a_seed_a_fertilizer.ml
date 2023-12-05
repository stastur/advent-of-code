open Base

let is_in_range n start length = n >= start && n < start + length

let convert seeds map =
  List.map seeds ~f:(fun seed ->
      let n =
        List.find_map map ~f:(fun (src, dest, len) ->
            if is_in_range seed src len then Some (seed - src + dest) else None)
      in
      match n with Some v -> v | None -> seed)

let parse_mapping line =
  Stdlib.Scanf.sscanf line "%d %d %d" (fun d s l -> (s, d, l))

let parse_seeds line =
  String.split ~on:':' line |> List.last_exn |> String.split ~on:' '
  |> List.filter_map ~f:Stdlib.int_of_string_opt

let parse_maps lines =
  let rec aux lines curr_map maps =
    match lines with
    | [] -> curr_map :: maps
    | "" :: lines -> aux lines [] (curr_map :: maps)
    | line :: lines when String.is_suffix line ~suffix:"map:" ->
        aux lines curr_map maps
    | line :: lines ->
        let range = parse_mapping line in
        aux lines (range :: curr_map) maps
  in
  aux lines [] [] |> List.rev

let parse_seed_ranges line =
  let nums = parse_seeds line in
  let rec aux nums ranges =
    match nums with
    | s :: l :: nums -> aux nums ((s, s + l - 1) :: ranges)
    | [] | _ -> ranges
  in
  aux nums []

let map_range range map =
  let rec aux ranges_to_check mapped_ranges left_mappings =
    match ranges_to_check with
    | [] -> mapped_ranges
    | (low, high) :: ranges_tl -> (
        match left_mappings with
        | [] -> mapped_ranges @ ranges_to_check
        | (src, dest, len) :: tl ->
            let m_low = src in
            let m_high = src + len - 1 in
            let to_dest n = n - src + dest in

            let to_check, mapped =
              if high < m_low || low > m_high then
                (* no overlap *)
                ([ (low, high) ], [])
              else if m_low <= low then
                if high <= m_high then
                  (* mapping contains range *)
                  ([], [ (to_dest low, to_dest high) ])
                else
                  (* range end is after mapping end  *)
                  ([ (m_high + 1, high) ], [ (to_dest low, to_dest m_high) ])
              else if low < m_low && high <= m_high then
                (* range start is before mapping start *)
                ( [ (low, m_low - 1); (high, m_high) ],
                  [ (to_dest m_low, to_dest m_high) ] )
              else ([ (low, m_low - 1) ], [ (to_dest m_low, to_dest high) ])
            in
            aux (to_check @ ranges_tl) (mapped @ mapped_ranges) tl)
  in
  aux [ range ] [] map

let () =
  let lines = Utils.read_lines () in
  let maps = List.drop lines 2 |> parse_maps in

  let part1 =
    let seeds = lines |> List.hd_exn |> parse_seeds in
    List.fold maps ~init:seeds ~f:convert
    |> List.min_elt ~compare |> Option.value_exn
  in

  let part2 =
    let ranges = lines |> List.hd_exn |> parse_seed_ranges in

    let planted_ranged =
      List.fold maps ~init:ranges ~f:(fun ranges map ->
          List.fold ranges ~init:[] ~f:(fun acc range ->
              acc @ map_range range map))
    in

    planted_ranged
    |> List.min_elt ~compare:(fun (a, _) (b, _) -> a - b)
    |> Option.value_exn |> fst
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
