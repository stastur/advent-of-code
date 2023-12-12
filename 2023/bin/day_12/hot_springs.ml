open Base

let contains list element = List.mem list element ~equal:Char.equal

let memo_rec f =
  let open Stdlib.Hashtbl in
  let h = create 16 in
  let rec g x =
    if not (mem h x) then add h x (f g x);
    find h x
  in
  g

let count_springs_permutations self (pattern, groups) =
  if List.is_empty pattern then List.is_empty groups |> Bool.to_int
  else if List.is_empty groups then contains pattern '#' |> not |> Bool.to_int
  else
    match pattern with
    | '.' :: chars -> self (chars, groups)
    | '?' :: chars -> self (chars, groups) + self ('#' :: chars, groups)
    | '#' :: chars -> (
        match groups with
        | n :: ns when n <= List.length pattern -> (
            let n = n - 1 in
            let curr_group, next_group = List.split_n chars n in

            if contains curr_group '.' then 0
            else
              match next_group with
              | ('?' | '.') :: next_group_tl -> self (next_group_tl, ns)
              | [] -> if List.is_empty ns then 1 else 0
              | _ -> 0)
        | _ -> 0)
    | _ -> failwith "invalid pattern"

let count_springs_permutations = memo_rec count_springs_permutations

let repeat str n sep =
  let rec aux acc n = if n = 0 then acc else aux (acc ^ sep ^ str) (n - 1) in
  aux str (n - 1)

let () =
  let lines = Utils.read_lines () in

  let part1 =
    List.fold lines ~init:0 ~f:(fun acc line ->
        match String.split ~on:' ' line with
        | [ condition; damaged ] ->
            acc
            + count_springs_permutations
                (String.to_list condition, Utils.to_int_list ~sep:',' damaged)
        | _ -> failwith "invalid input")
  in

  let part2 =
    List.fold lines ~init:0 ~f:(fun acc line ->
        match String.split ~on:' ' line with
        | [ condition; damaged ] ->
            let condition = repeat condition 5 "?" in
            let damaged = repeat damaged 5 "," in

            acc
            + count_springs_permutations
                (String.to_list condition, Utils.to_int_list ~sep:',' damaged)
        | _ -> failwith "invalid input")
  in
  Utils.print_result (Int.to_string part1) (Int.to_string part2)
