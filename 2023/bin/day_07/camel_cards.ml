open Base

let score_card card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '2' .. '9' -> Char.get_digit_exn card
  | _ -> failwith "Invalid card"

let score_card_with_jokers card =
  match card with 'J' -> -1 | _ -> score_card card

let score_hand ~score ~jokers hand =
  let groups =
    List.sort_and_group hand ~compare:(fun a b ->
        Int.compare (score b) (score a))
    |> List.map ~f:List.length
    |> List.sort ~compare:Int.compare
  in
  match groups with
  | [ 5 ] -> 7
  | [ 1; 4 ] -> if jokers = 0 then 6 else 7
  | [ 2; 3 ] -> if jokers = 0 then 5 else 7
  | [ 1; 1; 3 ] -> if jokers = 0 then 4 else 6
  | [ 1; 2; 2 ] -> if jokers = 0 then 3 else if jokers = 1 then 5 else 6
  | [ 1; 1; 1; 2 ] -> if jokers = 0 then 2 else 4
  | [ 1; 1; 1; 1; 1 ] -> if jokers = 0 then 1 else 2
  | _ -> failwith "Skill issue"

let compare_cards ~score a b =
  let cmp = ref 0 in
  for i = 0 to List.length a do
    if !cmp = 0 then
      let a_score = score (List.nth_exn a i) in
      let b_score = score (List.nth_exn b i) in
      cmp := Int.compare a_score b_score
  done;
  !cmp

let () =
  let lines = Utils.read_lines () in
  let hands =
    List.map lines ~f:(fun line ->
        Stdlib.Scanf.sscanf line "%s %d" (fun hand bid ->
            (String.to_list hand, bid)))
  in
  let part1 =
    let score = score_card in
    let compare_cards = compare_cards ~score in
    let score_hand hand = score_hand ~score ~jokers:0 hand in

    let compare_hands a b =
      Int.compare (score_hand a) (score_hand b) |> function
      | 0 -> compare_cards a b
      | v -> v
    in
    List.sort hands ~compare:(fun (a, _) (b, _) -> compare_hands a b)
    |> List.foldi ~init:0 ~f:(fun i acc (_, bid) -> acc + ((i + 1) * bid))
  in

  let part2 =
    let score = score_card_with_jokers in
    let compare_cards = compare_cards ~score in
    let score_hand hand =
      let jokers = List.count hand ~f:(Char.equal 'J') in
      score_hand ~score ~jokers hand
    in

    let compare_hands a b =
      Int.compare (score_hand a) (score_hand b) |> function
      | 0 -> compare_cards a b
      | v -> v
    in

    List.sort hands ~compare:(fun (a, _) (b, _) -> compare_hands a b)
    |> List.foldi ~init:0 ~f:(fun i acc (_, bid) -> acc + ((i + 1) * bid))
  in
  Utils.print_result (Int.to_string part1) (Int.to_string part2)
