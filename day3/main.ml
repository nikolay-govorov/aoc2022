open Base
open Stdio

let priority ch =
  let code = Caml.Char.code ch in
  if code >= 65 && code <= 90 then code - 38
  else if code >= 97 && code <= 122 then code - 96
  else assert false

let parse line = String.to_list line |> List.map ~f:priority

let intersect_pair l1 l2 =
  let hashmap = Array.create ~len:53 false in
  List.iter l1 ~f:(fun i -> hashmap.(i) <- true);
  List.filter l2 ~f:(fun i -> hashmap.(i))

let rec intersect lists =
  match lists with
  | [] -> []
  | hd :: [] -> hd
  | hd :: hd2 :: tl -> (intersect_pair hd hd2) :: tl |> intersect

let part1 priotities =
  let map items =
    let size = (List.length items) / 2 in
    let l1  = List.sub items ~pos:0 ~len:size in
    let l2 = List.sub items ~pos:size ~len:size in
    match intersect [l1; l2] with
    | hd :: _ -> hd
    | _ -> assert false in

  List.map priotities ~f:map |> List.fold ~init:0 ~f:(+)

let part2 priorities =
  let fold priorities line =
    match priorities with
    | current :: tl ->
      if List.length current < 3 then (line :: current) :: tl else [line] :: priorities
    | _ -> [[]]
  in

  let groups = List.fold priorities ~init:[[]] ~f:fold in
  let priorities = List.map groups ~f:(fun x -> match intersect x with | hd :: _ -> hd | _ -> assert false) in
  List.fold priorities ~init:0 ~f:(+)

let () =
  let lines = In_channel.read_lines "day3/input.txt" |> List.map ~f:parse in
  part1 lines |> printf "part1: %i\n";
  part2 lines |> printf "part2: %i\n";

