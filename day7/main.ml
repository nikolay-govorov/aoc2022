open Base
open Stdio

type fdnode = {
  dir: bool;
  size: int;
  mutable children: fdnode list;
}

type line =
  | Ls
  | Cd of { name: string }
  | Item of { name: string; dir: bool; size: int }

let parse_line line =
  let open Poly in

  match String.split line ~on:' ' with
  | p :: c :: [] when p = "$" && c = "ls" -> Ls
  | p :: c :: n :: [] when p = "$" && c = "cd" -> Cd { name = n }

  | p :: name :: [] ->
      if p = "dir" then
        Item { name; dir = true; size = 0 }
      else
        Item { name; dir = false; size = Int.of_string p }

  | _ -> assert false

let parse list =
  let open Poly in

  let commands = List.map list ~f:parse_line in

  (* parse tree only, without files *)
  let st = List.fold commands ~init:(Stack.create()) ~f:(fun st item ->
    match item with
    | Cd { name } when name = ".." ->
        let ch = Stack.pop_exn st in
        let last = Stack.top_exn st in
        last.children <- last.children @ [ch];
        st

    | Cd { name = _ } ->
        Stack.push st ({ dir = true; size = 0; children = [] });
        st

    | Item { name = _; dir; size } when not dir ->
        let last = Stack.top_exn st in
        let file = { dir = false; size = size; children = [] } in
        last.children <- file :: last.children;
        st

    | _ -> st
  ) in

  Stack.fold st
    ~init:(Stack.pop_exn st)
    ~f:(fun acc el -> 
      el.children <- el.children @ [acc];
      el)

let calc_dirs_sizes tree =
  let sizes = ref [] in

  let rec trpass tree =
    let size = List.map tree.children
      ~f:(fun it -> if it.dir then trpass it else it.size)
      |> List.fold ~init:0 ~f:(+)
    in
    sizes := size :: !sizes;
    size
  in

  let full_size = trpass tree in

  (full_size, !sizes)

let part1 tree =
  let (_, sizes) = calc_dirs_sizes tree in 
  List.filter sizes ~f:(fun it -> it <= 100_000) |> List.fold ~init:0 ~f:(+)

let part2 tree =
  let (full_size, sizes) = calc_dirs_sizes tree in 
  let aviavle_size = 70_000_000 - full_size in
  let need_size = 30_000_000 - aviavle_size in

  List.filter sizes ~f:(fun it -> it >= need_size) |> List.reduce ~f:min |> Option.value_exn

let () =
  let tree = In_channel.read_lines "day7/input.txt" |> parse in

  part1 tree |> printf "part1: %i\n";
  part2 tree |> printf "part2: %i\n";

