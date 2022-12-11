open Base
open Stdio

let parse_map src_map =
  let count_of_stacks =
    match src_map with
    | hd :: _ -> ((String.length hd) + 1) / 4
    | _ -> assert false
  in

  let map = Array.init count_of_stacks ~f:(fun _ -> Stack.create ()) in

  List.iter src_map ~f:(fun line -> (* Imperative ocaml! *)
    let i = ref 0 in
    while !i <= (String.length line) do
      if String.get line !i |> Char.equal '['
      then String.get line (!i + 1) |> Stack.push map.(!i / 4);

      i := !i + 4;
    done);

  map

let parse_commands commands =
  let parse ch = Int.of_string ch in
  List.map commands ~f:(fun line ->
    match String.split line ~on:' ' with
    | _ :: m :: _ :: f :: _ :: t :: [] -> (parse m, parse f, parse t)
    | _ -> assert false
  )

let parse lines =
  (* split map and commands *)
  let (src_map, src_commands) = List.fold
    lines
    ~init:([], [])
    ~f:(fun (m, c) line ->
      if Caml.String.starts_with line ~prefix:"move" then (m, line :: c)
      else if Caml.String.starts_with (String.strip line) ~prefix:"[" then (line :: m, c)
      else (m, c))
  in
  let src_commands = List.rev src_commands in

  let map = parse_map src_map in
  let commands = parse_commands src_commands in
  (map, commands)

let result map =
  let chs = Array.map map ~f:(fun st ->
    match Stack.pop st with
    | None -> assert false
    | Some(ch) -> ch)
  in
  Array.to_list chs |> String.of_char_list


let part1 (map, commands) =
  let map = Array.copy map |> Array.map ~f:(fun ct -> Stack.copy ct) in

  List.iter commands ~f:(fun (m, f, t) ->
    for _ = 0 to m - 1 do
      match Stack.pop map.(f - 1) with
      | None -> assert false
      | Some(ch) -> Stack.push map.(t - 1) ch
    done);

  result map

let part2 (map, commands) =
  let map = Array.copy map |> Array.map ~f:(fun ct -> Stack.copy ct) in

  let rec copy (m, f, t) =
    match m with 
    | 0 -> ()
    | _ ->
      let ch = match Stack.pop map.(f - 1) with
        | None -> assert false
        | Some(ch) -> ch
      in
        copy (m - 1, f, t);
        Stack.push map.(t - 1) ch;
  in

  List.iter commands ~f:copy;

  result map

let () =
  let data = In_channel.read_all "day5/input.txt" in
  let lines = String.split data ~on:'\n' |> parse in
  part1 lines |> printf "part1: %s\n";
  part2 lines |> printf "part2: %s\n";

