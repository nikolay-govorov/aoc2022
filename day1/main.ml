open Base
open Stdio

let parse (elves, sum) line =
  match line with
  | "" -> (sum :: elves, 0)
  | _ -> (elves, sum + Int.of_string line)

let part1 elves = List.fold elves ~init:0 ~f:max

let part2 elves =
  let max (m1, m2, m3) item =
    if item > m1 then (item, m1, m2)
    else if item > m2 then (m1, item, m2)
    else if item > m3 then (m1, m2, item)
    else (m1, m2, m3) in

  let (m1, m2, m3) = List.fold elves ~init:(0, 0, 0) ~f:max in
  m1 + m2 + m3

let () =
  let (elves, _) = In_channel.read_lines "day1/input.txt"
    |> List.fold ~init:([], 0) ~f:parse in
  part1 elves |> printf "part1: %i\n";
  part2 elves |> printf "part2: %i\n";

