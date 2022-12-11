open Base
open Stdio

let parse_gap gap =
  match String.split gap ~on:'-' with
  | hd :: tl :: [] -> (Int.of_string hd, Int.of_string tl)
  | _ -> assert false

let parse line =
  match String.split line ~on:',' with
  | hd :: tl :: [] -> (parse_gap hd, parse_gap tl)
  | _ -> assert false

let gap_contains (a1, a2) (b1, b2) =
  b1 >= a1 && b2 <= a2

let gap_overlap (a1, a2) (b1, b2) =
  b1 <= a2 && b2 >= a1

let part1 gaps =
  List.filter gaps ~f:(fun (a, b) -> (gap_contains a b) || (gap_contains b a)) |> List.length

let part2 gaps =
  List.filter gaps ~f:(fun (a, b) -> (gap_overlap a b) || (gap_overlap b a)) |> List.length

let () =
  let gaps = In_channel.read_lines "day4/input.txt"
    |> List.map ~f:parse in
  part1 gaps |> printf "part1: %i\n";
  part2 gaps |> printf "part2: %i\n";

