open Base
open Stdio

let code chr = (Caml.Char.code chr - 96) (* a -> 1, z -> 26*)

let parse lines =
  let bstart = ref (-1, -1) and bend = ref (-1, -1) in

  let board = List.to_array lines |> Array.mapi ~f:(fun x line ->
    String.to_array line |> Array.mapi ~f:(fun y ch ->
      match ch with
      | 'S' ->
          bstart := (x, y);
          code 'a'
      | 'E' ->
          bend := (x, y);
          code 'z'
      | chr -> code chr
    ))
  in

  (!bstart, !bend, board)

let search (sx, sy) (ex, ey) board =
  let xsize = Array.length board in
  let ysize = Array.length board.(0) in

  let map = Array.make_matrix Int.max_value ~dimx:xsize ~dimy:ysize in

  let rec _search (x, y) ~path =
    if x = ex && y = ey then (* success, it's end *)
      Some(path)
    else begin               (* mark and search more *)
      map.(x).(y) <- path;

      let next = [x + 1, y; x - 1, y; x, y + 1; x, y - 1] in

      List.filter next ~f:(fun (nx, ny) ->
        nx >= 0 && nx < xsize &&                (* unbound *)
        ny >= 0 && ny < ysize &&
        board.(nx).(ny) - board.(x).(y) <= 1 && (* no way *)
        map.(nx).(ny) > (path + 1)              (* there is a shorter way*)
      )
        |> List.filter_map ~f:(_search ~path:(path + 1))
        |> List.reduce ~f:min
    end
  in

  _search (sx, sy) ~path:0

let part1 (bstart, bend, board) =
  search bstart bend board |> Option.value_exn ~message:"Cell is unreachable"

let part2 (_bstart, bend, board) =
  let desired = code 'a' in
  let starts = ref [] in

  Array.iteri board ~f:(fun x line ->
    Array.iteri line ~f:(fun y ch ->
      if ch = desired then starts := (x, y) :: !starts));

  List.filter_map !starts ~f:(fun start -> search start bend board)
  |> List.reduce ~f:min
  |> Option.value_exn ~message:"Cell is unreachable"

let () =
  let lines = In_channel.read_lines "day12/input.txt" |> parse in

  part1 lines |> printf "part1: %i\n";
  part2 lines |> printf "part2: %i\n";

