open Base

let get_number_1 line =
  let rec walk line s e =
    match (line.[s] |> Char.get_digit, line.[e] |> Char.get_digit) with
    | None, None -> walk line (s + 1) (e - 1)
    | Some _, None -> walk line s (e - 1)
    | None, Some _ -> walk line (s + 1) e
    | Some a, Some b -> (a, b)
  in
  let a, b = walk line 0 (String.length line - 1) in
  (a * 10) + b

let get_number_2 line =
  let spelled_digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in

  let word_to_int = function
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> failwith "unknown word"
  in

  let rec walk line idx direction =
    match line.[idx] with
    | ch when Char.is_digit ch -> Char.get_digit_exn ch
    | _ -> (
        let segment =
          String.sub line ~pos:idx ~len:(String.length line - idx)
        in
        let word =
          List.find
            ~f:(fun prefix -> String.is_prefix segment ~prefix)
            spelled_digits
        in
        match word with
        | Some w -> word_to_int w
        | None -> walk line (idx + direction) direction)
  in
  let a = walk line 0 1 in
  let b = walk line (String.length line - 1) (-1) in

  (a * 10) + b

let () =
  let lines = Utils.read_lines () in

  let part1 =
    lines |> List.map ~f:get_number_1 |> List.fold_left ~init:0 ~f:( + )
  in
  let part2 =
    lines |> List.map ~f:get_number_2 |> List.fold_left ~init:0 ~f:( + )
  in

  Utils.print_result (Stdlib.string_of_int part1) (Stdlib.string_of_int part2)
