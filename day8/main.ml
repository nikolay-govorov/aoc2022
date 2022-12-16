open Base
open Stdio

let parse lines =
  List.to_array lines
    |> Array.map ~f:(fun line -> String.to_array line |> Array.map ~f:Char.to_int)

let part1 map =
  let x_size = Array.length map in
  let y_size = Array.length map.(0) in

  let rec find (sx, sy) ~i ~f =
    let (nx, ny) = i (sx, sy) in
    if nx < 0 || nx >= x_size then false else
    if ny < 0 || ny >= y_size then false else
    if f (nx, ny) then true
    else find (nx, ny) ~i ~f
  in

  let check_visible x y =
    let f = find (x, y) ~f:(fun (x0, y0) -> map.(x).(y) <= map.(x0).(y0)) in

    let left   = f ~i:(fun (x, y) -> (x - 1, y)) in
    let right  = f ~i:(fun (x, y) -> (x + 1, y)) in
    let top    = f ~i:(fun (x, y) -> (x, y - 1)) in
    let bottom = f ~i:(fun (x, y) -> (x, y + 1)) in

    not (left && right && top && bottom)
  in

  let visible = Array.mapi map
    ~f:(fun i line -> Array.mapi line ~f:(fun j _ -> check_visible i j))
  in

  Array.fold visible
    ~init:0
    ~f:(fun count line -> count + Array.fold line ~init:0 ~f:(fun acc i -> if i then acc + 1 else acc))

let part2 map =
  let x_size = Array.length map in
  let y_size = Array.length map.(0) in

  let rec find (sx, sy) ~i ~l ~f =
    let (nx, ny) = i (sx, sy) in
    if nx < 0 || nx >= x_size then l else
    if ny < 0 || ny >= y_size then l else
    if f (nx, ny) then l + 1
    else find (nx, ny) ~i ~l:(l + 1) ~f
  in

  let score x y =
    let f = find (x, y) ~l:0 ~f:(fun (x0, y0) -> map.(x0).(y0) >= map.(x).(y)) in

    let left   = f ~i:(fun (x, y) -> (x - 1, y)) in
    let right  = f ~i:(fun (x, y) -> (x + 1, y)) in
    let top    = f ~i:(fun (x, y) -> (x, y - 1)) in
    let bottom = f ~i:(fun (x, y) -> (x, y + 1)) in
    
    left * right * top * bottom
  in

  let scores = Array.mapi map
    ~f:(fun i line -> Array.mapi line ~f:(fun j _ -> score i j))
  in

  Array.fold scores ~init:0 ~f:(fun acc line -> Array.fold line ~init:0 ~f:max |> max acc)

let () =
  let map = In_channel.read_lines "day8/input.txt" |> parse in

  part1 map |> printf "part1: %i\n";
  part2 map |> printf "part2: %i\n";

