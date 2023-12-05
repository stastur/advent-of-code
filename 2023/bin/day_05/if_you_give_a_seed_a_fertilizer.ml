open Base

let is_in_range n start length = n >= start && n < start + length

let convert seeds map =
  List.map seeds ~f:(fun seed ->
      let n =
        List.find_map map ~f:(fun (src, dest, len) ->
            if is_in_range seed src len then Some (seed - src + dest) else None)
      in
      match n with Some v -> v | None -> seed)

let parse_range line =
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
        let range = parse_range line in
        aux lines (range :: curr_map) maps
  in
  aux lines [] [] |> List.rev

let () =
  let lines = Utils.read_lines () in
  let seeds = lines |> List.hd_exn |> parse_seeds in
  let maps = List.drop lines 2 |> parse_maps in

  let part1 =
    List.fold maps ~init:seeds ~f:convert
    |> List.min_elt ~compare |> Option.value_exn
  in

  Utils.print_result (Int.to_string part1) ""
