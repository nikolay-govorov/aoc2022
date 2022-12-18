open Base
open Stdio

let parse commands = 
  let (_, log) = List.fold commands
    ~init:(1, [])
    ~f:(fun (current, log) line ->
      match String.split line ~on:' ' with
        | "noop" :: [] -> (current, current :: log)
        | "addx" :: size :: [] -> (current + Int.of_string size, current :: current :: log)
        | _ -> assert false
    )
  in
  log |> List.rev |> List.to_array

let part1 log = [20; 60; 100; 140; 180; 220] |>
  List.fold ~init:0 ~f:(fun s i -> s + log.(i - 1) * i)

let part2 log =
  let width = 6 and height = 40 in
  let board = Array.make_matrix " " ~dimx:width ~dimy:height in

  (* Make board *)
  Array.iteri log ~f:(fun cycle register ->
    let x = cycle / height and y = cycle % height in

    if register = y || register - 1 = y || register + 1 = y then
      board.(x).(y) <- "#";
  );

  (* render board *)
  Array.map board ~f:String.concat_array |> String.concat_array ~sep:"\n" |> printf "%s\n"

let () =
  let log = In_channel.read_lines "day10/input.txt" |> parse in

  part1 log |> printf "part1: %i\n";

  printf "part2:\n";
  part2 log;

