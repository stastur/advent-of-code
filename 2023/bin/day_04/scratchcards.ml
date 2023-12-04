open Base

let parse_line line =
  let string_to_list_of_ints s =
    String.split s ~on:' '
    |> List.filter_map ~f:(function "" -> None | s -> Some (Int.of_string s))
  in
  let nums, win_nums =
    line |> String.split ~on:':' |> List.last_exn |> String.split ~on:'|'
    |> function
    | [ a; b ] -> (string_to_list_of_ints a, string_to_list_of_ints b)
    | _ -> failwith "invalid input"
  in
  (nums, win_nums)

let intersection a b = List.filter a ~f:(fun n -> List.mem b n ~equal:( = ))

let score = function [] -> 0 | list -> Int.pow 2 (List.length list - 1)

let () =
  let cards = Utils.read_lines () |> List.map ~f:parse_line in
  let part1 =
    cards
    |> List.map ~f:(fun (nums, win_nums) -> score (intersection nums win_nums))
    |> List.sum (module Int) ~f:Fn.id
  in

  let part2 =
    let rec scratch current_idx cards deck =
      match cards with
      | [] -> deck
      | (nums, win_nums) :: tl ->
          let n = intersection nums win_nums |> List.length in
          let n_of_copies = List.nth_exn deck current_idx in
          let updated_deck =
            List.mapi deck ~f:(fun i count ->
                if current_idx < i && i <= current_idx + n then
                  count + n_of_copies
                else count)
          in
          scratch (current_idx + 1) tl updated_deck
    in

    let initial_deck = List.init ~f:(fun _ -> 1) (List.length cards) in
    scratch 0 cards initial_deck |> List.sum (module Int) ~f:Fn.id
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
