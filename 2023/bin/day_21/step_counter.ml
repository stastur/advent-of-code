open Base

let directions = [ (1, 0); (0, 1); (-1, 0); (0, -1) ]

let walk grid steps (x, y) =
  let w = Array.length grid.(0) in
  let h = Array.length grid in

  let visited =
    Hashtbl.create
      (module struct
        type t = int * int * int [@@deriving hash, compare, sexp_of]
      end)
  in

  let count = ref 0 in

  let rec aux steps (x, y) =
    if Hashtbl.mem visited (x, y, steps) then ()
    else (
      Hashtbl.set visited ~key:(x, y, steps) ~data:();

      if Char.(grid.(y % h).(x % w) = '#') then ()
      else if steps = 0 then Int.incr count
      else
        List.iter directions ~f:(fun (dx, dy) ->
            let x = x + dx in
            let y = y + dy in
            aux (steps - 1) (x, y)))
  in

  aux steps (x, y);
  !count

let find_fx y0 y1 y2 =
  (*
    y = ax^2 + bx + c
    y0 = c
    y1 = a + b + c
    y2 = 4a + b + c
  *)
  let a = (y2 - (2 * y1) + y0) / 2 in
  let b = y1 - y0 - a in
  let c = y0 in

  fun x -> (a * (x ** 2)) + (b * x) + c

let () =
  let grid =
    Utils.read_lines () |> List.to_array |> Array.map ~f:String.to_array
  in
  let start =
    let y, row =
      Array.findi_exn grid ~f:(fun _ r -> Array.mem r 'S' ~equal:Char.equal)
    in
    let x, _ = Array.findi_exn row ~f:(fun _ c -> Char.equal c 'S') in
    (x, y)
  in

  let part1 = walk grid 6 start in

  let part2 =
    (* Not without a help: https://www.reddit.com/r/adventofcode/comments/18njrqf/2023_day_21_a_diamond_in_the_rough/ *)
    (* let n = 26_501_365 in
       let size = Array.length grid in
       let cycles = n / size in
       let offset = n % size in

       let y0 = walk grid offset start in
       let y1 = walk grid (size + offset) start in
       let y2 = walk grid ((2 * size) + offset) start in

       let fx = find_fx y0 y1 y2 in

       fx cycles *)
    0
  in

  Utils.print_result (Int.to_string part1) (Int.to_string part2)
