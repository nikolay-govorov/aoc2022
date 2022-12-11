open Base
open Stdio

let parse line =
  match String.split line ~on:' ' with
  | opp :: me :: [] -> (opp, me)
  | _ -> assert false

let part1 (opponent, me) =
  let point_for_turn =
    match me with
    | "X" -> 1
    | "Y" -> 2
    | "Z" -> 3
    | _ -> assert false in

  let point_for_win =
    match opponent with
    (* A defeats C, C defeats B, and B defeats A  *)
    | "A" -> (match me with | "Y" -> 6 | "X" -> 3 | _ -> 0)
    | "B" -> (match me with | "Z" -> 6 | "Y" -> 3 | _ -> 0)
    | "C" -> (match me with | "X" -> 6 | "Z" -> 3 | _ -> 0)
    | _ -> assert false in

  point_for_turn + point_for_win

let part2 (opponent, result) =
  let points_for_step =
    match opponent with
    | "A" -> (match result with | "Z" -> (* B *) 2 | "Y" -> (* A *) 1 | "X" -> (* C *) 3 | _ -> assert false)
    | "B" -> (match result with | "Z" -> (* C *) 3 | "Y" -> (* B *) 2 | "X" -> (* A *) 1 | _ -> assert false)
    | "C" -> (match result with | "Z" -> (* A *) 1 | "Y" -> (* C *) 3 | "X" -> (* B *) 2 | _ -> assert false)
    | _ -> assert false in

  let points_for_result =
    match result with
    | "X" -> 0 (* lose *)
    | "Y" -> 3 (* draw *)
    | "Z" -> 6 (* win *)
    | _ -> assert false in

  points_for_step + points_for_result

let calc points ~f =
  List.map points ~f |> List.fold ~init:0 ~f:(+)

let () =
  let lines = In_channel.read_lines "day2/input.txt" |> List.map ~f:parse in
  calc ~f:part1 lines |> printf "part1: %i\n"; (* 11603 *)
  calc ~f:part2 lines |> printf "part2: %i\n"; (* 11603 *)

