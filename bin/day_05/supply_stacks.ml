open Base

let space_between_stacks = 4

type cmd = { src : int; dst : int; count : int }

let cmd_from line =
  Stdlib.Scanf.sscanf line "move %d from %d to %d" (fun count src dst ->
      { src = src - 1; dst = dst - 1; count })

let parse_stacks lines =
  List.map ~f:String.to_list lines
  |> List.map ~f:(List.filteri ~f:(fun i _ -> i % space_between_stacks = 1))
  |> List.transpose_exn
  |> List.map ~f:(fun stack ->
         stack |> List.drop_while ~f:Char.is_whitespace |> List.rev)

let parse_commands lines = List.map ~f:cmd_from lines

let get_load stacks cmd =
  let src_stack = List.nth_exn stacks cmd.src in

  List.filteri
    ~f:(fun i _ -> i > List.length src_stack - cmd.count - 1)
    src_stack

let place stacks load src dst =
  List.mapi
    ~f:(fun i stack ->
      match i with
      | _ when i = src ->
          List.filteri
            ~f:(fun idx _ -> idx < List.length stack - List.length load)
            stack
      | _ when i = dst -> stack @ load
      | _ -> stack)
    stacks

let mover_9000 stacks cmd =
  let crates = get_load stacks cmd in
  place stacks (List.rev crates) cmd.src cmd.dst

let mover_9001 stacks cmd =
  let crates = get_load stacks cmd in
  place stacks crates cmd.src cmd.dst

let parse_lines lines =
  let stack_lines, command_lines =
    List.findi_exn ~f:(fun _ line -> String.is_empty line) lines
    |> fun (n, _) ->
    List.split_n lines n |> fun (stacks, commands) ->
    (List.drop_last_exn stacks, List.tl_exn commands)
  in

  (parse_stacks stack_lines, parse_commands command_lines)

let top_crates stacks =
  List.fold_left
    ~f:(fun acc stack ->
      match List.last stack with Some last -> last :: acc | None -> acc)
    ~init:[] stacks
  |> List.rev

let use_crane ~model ~stacks ~commands =
  List.fold_left ~f:model ~init:stacks commands
  |> top_crates |> String.of_char_list

let () =
  let lines = Utils.read_lines () in
  let stacks, commands = parse_lines lines in

  Utils.print_result
    (use_crane ~model:mover_9000 ~stacks ~commands)
    (use_crane ~model:mover_9001 ~stacks ~commands)
