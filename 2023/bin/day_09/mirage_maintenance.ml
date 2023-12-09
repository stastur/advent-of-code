open Base

let rec next_diff_sequence = function
  | hd :: next :: tl -> (next - hd) :: next_diff_sequence (next :: tl)
  | _ -> []

let contains_only_zeros = List.for_all ~f:(Int.equal 0)

let process_history nums =
  let rec aux diff_seq acc =
    if contains_only_zeros diff_seq then acc
    else
      let next_seq = next_diff_sequence diff_seq in
      aux next_seq (next_seq :: acc)
  in
  aux nums (nums :: [])

let sum = List.fold ~init:0 ~f:( + )

let extrapolate =
  List.fold ~init:0 ~f:(fun acc nums -> acc + List.last_exn nums)

let extrapolate_backwards =
  List.fold ~init:0 ~f:(fun acc nums -> List.hd_exn nums - acc)

let () =
  let lines = Utils.read_lines () in
  let histories =
    List.map lines
      ~f:(Fn.compose (List.map ~f:Int.of_string) (String.split ~on:' '))
  in

  let part1 =
    List.map histories ~f:(Fn.compose extrapolate process_history) |> sum
  in
  let part2 =
    List.map histories ~f:(Fn.compose extrapolate_backwards process_history)
    |> sum
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
