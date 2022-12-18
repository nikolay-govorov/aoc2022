open Base
open Stdio

(* block: utils *)
let rec unique list =
  let open Poly in

  let rec help l n =
    match l with
    | [] -> []
    | hd :: tl -> if n = hd then help tl n else hd :: help tl n
  in

  match list with
  | [] -> []
  | hd :: tl -> hd :: (help (unique tl) hd)

let hypotenuse dx dy : float =
  let dx2 = Float.of_int (dx ** 2) in
  let dy2 = Float.of_int (dy ** 2) in
  Float.sqrt (dx2 +. dy2)

let inc x y = x + if y > 0 then 1 else - 1

(* block: parser *)
type move = Left | Right | Up | Down

let parse lines =
  List.map lines ~f:(fun line ->
    match String.split line ~on:' ' with
    | "L" :: size :: [] -> (Left, Int.of_string size)
    | "R" :: size :: [] -> (Right, Int.of_string size)
    | "U" :: size :: [] -> (Up, Int.of_string size)
    | "D" :: size :: [] -> (Down, Int.of_string size)
    | _ -> assert false
  )

(* block: emulator *)
type t = {
  nodes: (int * int) list;
  log: (int * int) list;
}

let rec last = function
  | [] -> assert false
  | [it] -> it
  | _ :: tl -> last tl

let make len =
  let nodes = List.init len ~f:(fun _ -> (0, 0)) in
  { nodes; log = [(0, 0)] }

let rec move_tail (hx, hy) nodes ~dir =
  let open Poly in

  match nodes with
  | [] -> []
  | (tx, ty) :: tl ->
    let x_diff = hx - tx in
    let y_diff = hy - ty in

    let head =
      match (Int.abs x_diff, Int.abs y_diff) with
      | (2, 0) -> (inc tx x_diff, ty)
      | (0, 2) -> (tx, inc ty y_diff)
      | _ ->
        if hypotenuse x_diff y_diff > 2.0 then
          (inc tx x_diff, inc ty y_diff)
        else
          (tx, ty)
    in

    head :: (move_tail head tl ~dir)

let move rope (dir, size) =
  let rope = ref rope in

  for _ = 1 to size do
    rope :=  match !rope.nodes with
    | [] -> assert false
    | (x, y) :: tl ->
      let head = match dir with
        | Right -> (x + 1, y)
        | Left  -> (x - 1, y)
        | Up    -> (x, y + 1)
        | Down  -> (x, y - 1)
      in

      let nodes = head :: move_tail head tl ~dir in
      {
        nodes;
        log = (last nodes) :: !rope.log;
      }
  done;

  !rope

let () =
  let moves = In_channel.read_lines "day9/input.txt" |> parse in

  let calc ~size = 
    let rope = List.fold moves ~init:(make size) ~f:move in
    unique rope.log |> List.length
  in

  calc ~size:2 |> printf "part1: %i\n";
  calc ~size:10 |> printf "part2: %i\n";

