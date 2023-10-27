open Base

type file = { size : int }

type dir = {
  name : string;
  parent : dir option;
  mutable files : file list;
  mutable dirs : dir list;
}

let contains dir name =
  List.find ~f:(fun dir -> String.equal dir.name name) dir.dirs

let mkdir ?(files = []) ?(dirs = []) name parent = { name; parent; files; dirs }

let parse_lines lines =
  let root = { name = "/"; parent = None; files = []; dirs = [] } in

  let rec explore lines node =
    match lines with
    | [] -> node
    | hd :: tl -> (
        match String.split ~on:' ' hd with
        | [ "$"; "cd"; dir ] -> (
            match dir with
            | "/" -> explore tl root
            | ".." -> node.parent |> Option.value_exn |> explore tl
            | _ -> (
                match contains node dir with
                | Some dir -> explore tl dir
                | None ->
                    let new_dir = mkdir dir (Some node) in
                    node.dirs <- new_dir :: node.dirs;
                    explore tl node))
        | [ "dir"; dir ] ->
            node.dirs <- mkdir dir (Some node) :: node.dirs;
            explore tl node
        | [ "$"; "ls" ] -> explore tl node
        | [ size; _ ] ->
            node.files <- { size = Stdlib.int_of_string size } :: node.files;
            explore tl node
        | _ -> failwith "Invalid command")
  in
  ignore (explore lines root);
  root

let rec size dir =
  List.sum (module Int) ~f:(fun file -> file.size) dir.files
  + List.sum (module Int) ~f:size dir.dirs

let rec size_folders root =
  size root :: List.concat_map ~f:size_folders root.dirs

let get_in_range ~min ~max sizes =
  sizes |> List.filter ~f:(fun size -> min <= size && size <= max)

let () =
  let root = Utils.read_lines () |> parse_lines in
  let sizes = size_folders root in

  let part1 =
    sizes
    |> get_in_range ~min:0 ~max:100_000
    |> List.fold_left ~f:( + ) ~init:0
    |> Stdlib.string_of_int
  in

  let space_total = 70_000_000
  and space_required = 30_000_000
  and space_taken = size root in
  let space_to_free = space_required + space_taken - space_total in

  let part2 =
    sizes
    |> get_in_range ~min:space_to_free ~max:space_taken
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn |> Stdlib.string_of_int
  in

  Utils.print_result part1 part2
