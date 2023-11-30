open Base
open Stdlib.Scanf

module Monkey = struct
  type test = { divisible_by : int; yes : int; no : int }

  type t = {
    mutable n : int;
    items : int Queue.t;
    test : test;
    inspect : int -> int;
  }

  let from_lines lines =
    let parse_items line =
      String.split ~on:':' line |> List.last_exn |> String.split ~on:','
      |> List.map ~f:(Fn.compose Int.of_string String.strip)
      |> Queue.of_list
    in

    let parse_operation line =
      let parse_operand op value =
        if String.equal op "old" then value else Int.of_string op
      in

      let left_operand, operator, right_operand =
        sscanf line "Operation: new = %s %c %s" (fun l op r -> (l, op, r))
      in

      fun old ->
        let operator =
          match operator with
          | '+' -> ( + )
          | '*' -> ( * )
          | _ -> failwith "Unsupported operator"
        in

        let left = parse_operand left_operand old in
        let right = parse_operand right_operand old in

        operator left right
    in

    let parse_test lines =
      match lines with
      | [ test; if_true; if_false ] ->
          {
            divisible_by = sscanf test "Test: divisible by %d" Fn.id;
            yes = sscanf if_true "If true: throw to monkey %d" Fn.id;
            no = sscanf if_false "If false: throw to monkey %d" Fn.id;
          }
      | _ -> failwith "Failed to build test"
    in

    match lines with
    | _ :: items_line :: op_line :: test_lines ->
        {
          items = parse_items items_line;
          inspect = parse_operation op_line;
          test = parse_test test_lines;
          n = 0;
        }
    | _ -> failwith "Failed to create monkey"

  let pass_to_next monkey monkeys item =
    monkey.n <- monkey.n + 1;

    let next_idx =
      if item % monkey.test.divisible_by = 0 then monkey.test.yes
      else monkey.test.no
    in

    match List.nth monkeys next_idx with
    | Some m -> Queue.enqueue m.items item
    | None -> ()
end

let parse_lines lines =
  let rec aux lines buff monkeys =
    match lines with
    | "" :: tl -> Monkey.from_lines (List.rev buff) :: aux tl [] monkeys
    | hd :: tl -> aux tl (String.strip hd :: buff) monkeys
    | [] -> Monkey.from_lines (List.rev buff) :: monkeys
  in

  aux lines [] []

let () =
  let lines = Utils.read_lines () in

  let monkey_business (monkeys : Monkey.t list) =
    let rec aux (monkeys : Monkey.t list) first second =
      match monkeys with
      | [] -> (first, second)
      | hd :: tl ->
          if hd.n > first then aux tl hd.n first
          else if hd.n > second then aux tl first hd.n
          else aux tl first second
    in
    let first, second = aux monkeys 0 0 in
    first * second
  in

  let part1 =
    let monkeys = parse_lines lines in
    for _ = 1 to 20 do
      List.iter
        ~f:(fun m ->
          while not (Queue.is_empty m.items) do
            let item = Queue.dequeue_exn m.items in
            let worry_lvl = m.inspect item / 3 in
            Monkey.pass_to_next m monkeys worry_lvl
          done)
        monkeys
    done;
    monkey_business monkeys
  in

  let part2 =
    let monkeys = parse_lines lines in
    let mods =
      List.fold ~f:(fun k m -> m.test.divisible_by * k) ~init:1 monkeys
    in

    let monkeys = parse_lines lines in
    for _ = 1 to 10_000 do
      List.iter
        ~f:(fun m ->
          while not (Queue.is_empty m.items) do
            let item = Queue.dequeue_exn m.items in
            let worry_lvl = m.inspect item % mods in
            Monkey.pass_to_next m monkeys worry_lvl
          done)
        monkeys
    done;
    monkey_business monkeys
  in

  Utils.print_result (Stdlib.string_of_int part1) (Stdlib.string_of_int part2)
