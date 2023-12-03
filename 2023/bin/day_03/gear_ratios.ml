open Base

module Schematics = struct
  type engine_part = {
    number : int;
    line_pos : int;
    start_pos : int;
    end_pos : int;
  }

  let of_strings_list strings = List.map ~f:String.to_list strings

  let get schematics x y =
    let open Option in
    List.nth schematics y >>= fun line -> List.nth line x

  let is_symbol = function '.' | '0' .. '9' -> false | _ -> true

  let directions =
    [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) ]

  let has_adjacent_symbol schematics part =
    let found = ref false in
    for i = part.start_pos to part.end_pos do
      List.iter directions ~f:(fun (dx, dy) ->
          let x = i + dx in
          let y = part.line_pos + dy in
          match get schematics x y with
          | Some ch when is_symbol ch -> found := true
          | _ -> ())
    done;
    !found

  let get_engine_parts schematics =
    let rec parts_from_line line line_pos start_pos acc =
      if start_pos >= List.length line then acc
      else if start_pos |> List.nth_exn line |> Char.is_digit |> not then
        parts_from_line line line_pos (start_pos + 1) acc
      else
        let digits =
          List.sub line ~pos:start_pos ~len:(List.length line - start_pos)
          |> List.take_while ~f:Char.is_digit
        in
        let number = digits |> String.of_char_list |> Int.of_string in
        let end_pos = start_pos + List.length digits - 1 in
        let part = { number; line_pos; start_pos; end_pos } in

        parts_from_line line line_pos (end_pos + 1) (part :: acc)
    in

    List.concat_mapi schematics ~f:(fun line_pos line ->
        let parts = parts_from_line line line_pos 0 [] in
        List.filter parts ~f:(has_adjacent_symbol schematics))

  let sum_part_numbers parts =
    List.sum (module Int) parts ~f:(fun { number; _ } -> number)

  let get_gear_positions schematics =
    List.concat_mapi schematics ~f:(fun y ->
        List.filter_mapi ~f:(fun x -> function '*' -> Some (x, y) | _ -> None))

  let get_gear_ratios gear_positions parts =
    let is_adjacent (x, y) { line_pos; start_pos; end_pos; _ } =
      let is_adj_x = start_pos - 1 <= x && x <= end_pos + 1 in
      let is_adj_y = y - 1 <= line_pos && line_pos <= y + 1 in
      is_adj_x && is_adj_y
    in

    List.filter_map gear_positions ~f:(fun (gx, gy) ->
        let adjacent_parts = List.filter parts ~f:(is_adjacent (gx, gy)) in
        match adjacent_parts with
        | [ p1; p2 ] -> Some (p1.number * p2.number)
        | _ -> None)

  let sum_gear_ratios gear_ratios = List.sum (module Int) ~f:Fn.id gear_ratios
end

let () =
  let lines = Utils.read_lines () in
  let schematics = Schematics.of_strings_list lines in
  let engine_parts = Schematics.get_engine_parts schematics in

  let part1 = Schematics.sum_part_numbers engine_parts in
  let part2 =
    let gear_positions = Schematics.get_gear_positions schematics in
    let gear_ratios = Schematics.get_gear_ratios gear_positions engine_parts in
    Schematics.sum_gear_ratios gear_ratios
  in

  Utils.print_result (Stdlib.string_of_int part1) (Stdlib.string_of_int part2)
