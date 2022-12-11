open Base
open Stdio

let is_uniq subarr =
  List.fold
    subarr
    ~init:true
    ~f:(fun acc i -> List.filter subarr ~f:(fun j -> Char.equal i j) |> List.length |> (=) 1 |> (&&) acc)

let rec find buffer size index =
  let subarr = List.sub buffer ~pos:index ~len:size in
  if is_uniq subarr then index + size else find buffer size (index + 1)

let part1 buffer = find buffer 4 0
let part2 buffer = find buffer 14 0

let () =
  let buffer = In_channel.read_all "day6/input.txt" |> String.to_list in

  part1 buffer |> printf "part1: %i\n"; (* 1892 *)
  part2 buffer |> printf "part2: %i\n";

