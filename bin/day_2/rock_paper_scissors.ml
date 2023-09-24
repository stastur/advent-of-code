type rps = Rock | Paper | Scissors

let from_str s =
  match s with
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | _ -> failwith "Invalid input"

let from_elf_str s =
  match s with
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "Invalid input"

let win move =
  match move with Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let score_move move = match move with Rock -> 1 | Paper -> 2 | Scissors -> 3

let score_outcome elf player =
  match player with
  | _ when player = win elf -> 6
  | _ when player = elf -> 3
  | _ -> 0

let part_1 elf_move player_move =
  let elf = from_elf_str elf_move in
  let player = from_str player_move in
  score_outcome elf player + score_move player

let lose move = move |> win |> win

let part_2 elf_move ending =
  let elf = from_elf_str elf_move in
  let player =
    match ending with
    | "X" -> lose elf
    | "Y" -> elf
    | "Z" -> win elf
    | _ -> failwith "Invalid input"
  in
  score_outcome elf player + score_move player

let parse_line l = (String.sub l 0 1, String.sub l 2 1)

let sum_rounds score_round rounds =
  List.fold_left
    (fun acc (elf_move, player_move) -> acc + score_round elf_move player_move)
    0 rounds

let () =
  let lines = Utils.read_lines () in
  let guide = List.map parse_line lines in

  Utils.print_result
    (guide |> sum_rounds part_1 |> string_of_int)
    (guide |> sum_rounds part_2 |> string_of_int)
