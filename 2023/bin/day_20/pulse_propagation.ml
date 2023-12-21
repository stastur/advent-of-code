open Base

let parse_line line =
  match
    String.split_on_chars line ~on:[ ' '; ',' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | name :: _arrow :: outputs -> (name, outputs)
  | _ -> failwith "Invalid format"

type t =
  | FlipFlop of { name : string; state : bool; outputs : string list }
  | Conjunction of {
      name : string;
      memory : (string * bool) list;
      outputs : string list;
    }

let parse_modules data =
  let get_inputs name =
    List.filter_map data ~f:(fun (n, outputs) ->
        if List.mem outputs name ~equal:String.equal then
          Some (String.drop_prefix n 1)
        else None)
  in

  let h_nodes = Hashtbl.create (module String) in
  let h_inputs = Hashtbl.create (module String) in

  List.iter data ~f:(fun (name, outputs) ->
      let prefix = String.get name 0 in
      let name = String.drop_prefix name 1 in

      List.iter outputs ~f:(fun n ->
          Hashtbl.add_multi h_inputs ~key:n ~data:name);

      match prefix with
      | '%' ->
          let node = FlipFlop { name; state = false; outputs } in
          Hashtbl.set h_nodes ~key:name ~data:node
      | '&' ->
          let memory = get_inputs name |> List.map ~f:(fun n -> (n, false)) in
          let node = Conjunction { name; outputs; memory } in
          Hashtbl.set h_nodes ~key:name ~data:node
      | _ -> ());
  (h_nodes, h_inputs)

let send_pulse ?(lookup = ignore) receivers modules =
  let lows = ref 0 in
  let highs = ref 0 in
  let pulseQ = Queue.create () in

  Queue.enqueue pulseQ (None, false, receivers);

  while not (Queue.is_empty pulseQ) do
    let src, pulse, receivers = Queue.dequeue_exn pulseQ in

    List.iter receivers ~f:(fun receiver ->
        if pulse then Int.incr highs else Int.incr lows;

        match Hashtbl.find modules receiver with
        | Some (FlipFlop { name; state; outputs } as node) ->
            lookup node;

            if not pulse then (
              let state = not state in
              let updated = FlipFlop { name; state; outputs } in

              Hashtbl.set modules ~key:name ~data:updated;
              Queue.enqueue pulseQ (Some name, state, outputs))
        | Some (Conjunction { name; memory; outputs } as node) -> (
            lookup node;

            match src with
            | Some src ->
                let memory =
                  List.map memory ~f:(fun saved ->
                      let name = fst saved in
                      if String.(name = src) then (name, pulse) else saved)
                in

                let out_pulse = not (List.for_all memory ~f:snd) in
                let updated = Conjunction { name; memory; outputs } in

                Hashtbl.set modules ~key:name ~data:updated;
                Queue.enqueue pulseQ (Some name, out_pulse, outputs)
            | None -> failwith "Unknown pulse source")
        | None -> ())
  done;

  (!lows, !highs)

let () =
  let data = Utils.read_lines () |> List.map ~f:parse_line in
  let _, system_inputs =
    List.find_exn data
      ~f:(Fn.compose (String.is_prefix ~prefix:"broadcaster") fst)
  in

  let part1 =
    let low = ref 0 in
    let high = ref 0 in
    let h_nodes, _ = parse_modules data in

    for _ = 1 to 1000 do
      let l, h = send_pulse system_inputs h_nodes in
      low := !low + l + 1;
      high := !high + h
    done;

    !low * !high
  in

  let part2 =
    let i = ref 0 in
    let h_nodes, h_inputs = parse_modules data in

    let rec find_key_node_name name =
      match Hashtbl.find h_inputs name with
      | Some inputs when List.length inputs > 1 -> Some name
      | Some [ input ] -> find_key_node_name input
      | _ -> None
    in

    let key_node_name = find_key_node_name "rx" in

    match key_node_name with
    | Some key_node_name ->
        let counts = Hashtbl.create (module String) in
        let inputs = Hashtbl.find_exn h_inputs key_node_name in

        let check_key_node = function
          | Conjunction n when String.(n.name = key_node_name) ->
              List.iter n.memory ~f:(fun (key, pulse) ->
                  if pulse then Hashtbl.set counts ~key ~data:!i)
          | _ -> ()
        in

        while not (List.for_all inputs ~f:(Hashtbl.mem counts)) do
          Int.incr i;
          ignore (send_pulse system_inputs h_nodes ~lookup:check_key_node)
        done;

        Utils.lcm_of_list (Hashtbl.data counts)
    | _ -> 0
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
