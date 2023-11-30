open Base

let process instructions on_tick =
  let rec aux instructions x c skipped_prev =
    match instructions with
    | [] -> ()
    | hd :: tl ->
        on_tick x c;
        let x, remaining, skipped_prev =
          match String.split ~on:' ' hd with
          | [ "noop" ] -> (x, tl, false)
          | [ "addx"; n ] ->
              if skipped_prev then (x + Int.of_string n, tl, false)
              else (x, hd :: tl, true)
          | _ -> failwith "invalid instruction"
        in
        let c = c + 1 in
        aux remaining x c skipped_prev
  in
  aux instructions 1 1 false

let evaluate_signal x c = if (c - 20) % 40 = 0 then x * c else 0

let calc_signal_strength instructions =
  let strength = ref 0 in
  process instructions (fun x c -> strength := !strength + evaluate_signal x c);
  !strength

let screen_width = 40

let print_crt instructions =
  process instructions (fun x c ->
      let pos = (c - 1) % screen_width in
      let pixel = if x - 1 <= pos && pos <= x + 1 then '#' else '.' in
      Stdlib.print_char pixel;
      if c % screen_width = 0 then Stdlib.print_newline ())

let () =
  let lines = Utils.read_lines () in
  let part1 = calc_signal_strength lines in
  Utils.print_result (Stdlib.string_of_int part1) "\n";
  print_crt lines
