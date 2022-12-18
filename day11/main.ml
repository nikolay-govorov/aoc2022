open Base
open Stdio

(* parse *)
type argument = Old | Explicit of int

type monkey = {
  _id: int;

  mutable checks: int;

  mutable items: int list;
  operation: (int -> int -> int) * argument * argument;

  test_divider: int;
  test_goal_true: int;
  test_goal_false: int;
}

let record line ~prefix ~suffix =
  String.strip line |> String.chop_prefix_exn ~prefix |> String.chop_suffix_exn ~suffix

let arg = function
  | "old" -> Old
  | x -> Explicit (Int.of_string x)

let parse_monkey lines =
  let lines = List.rev_map lines ~f:(String.strip) in
  match lines with
  | [monkey; items; operation; test; test_true; test_false] ->
      let monkey =     record monkey     ~prefix:"Monkey " ~suffix:":" in
      let items =      record items      ~prefix:"Starting items: " ~suffix:"" in
      let operation =  record operation  ~prefix:"Operation: new = " ~suffix:"" in
      let test =       record test       ~prefix:"Test: divisible by " ~suffix:"" in
      let test_true =  record test_true  ~prefix:"If true: throw to monkey " ~suffix:"" in
      let test_false = record test_false ~prefix:"If false: throw to monkey " ~suffix:"" in

      {
        _id = Int.of_string monkey;

        checks = 0;

        items = String.split items ~on:','
          |> List.map ~f:(fun it -> String.strip it |> Int.of_string);
        operation = (
          match String.split operation ~on:' ' with
          | [a; "+"; b] -> (( + ), arg a, arg b)
          | [a; "-"; b] -> (( - ), arg a, arg b)
          | [a; "*"; b] -> (( * ), arg a, arg b)
          | [a; "/"; b] -> (( / ), arg a, arg b)
          | _ -> assert false
        );

        test_divider = Int.of_string test;
        test_goal_true = Int.of_string test_true;
        test_goal_false = Int.of_string test_false;
      }

  | _ -> assert false

let parse_line (monkeys, current) line =
  match line with
  | "" -> ((parse_monkey current) :: monkeys, [])
  | _ -> (monkeys, line :: current)

let parse filename =
  let (monkeys, _) = In_channel.read_lines filename 
    |> List.fold ~init:([], []) ~f:parse_line
  in
  List.rev monkeys |> Array.of_list

(* emulate *)
let operand ~old = function
  | Old -> old
  | Explicit x -> x

let emulate monkeys ~count ~calc_worry =
  for _ = 1 to count do
    Array.iter monkeys ~f:(fun monkey ->
      let (operation, a, b) = monkey.operation in

      List.iter monkey.items ~f:(fun old ->
        let worry = calc_worry a b ~op:operation ~old in

        let goal = if (worry % monkey.test_divider) = 0
          then monkey.test_goal_true
          else monkey.test_goal_false
        in

        monkeys.(goal).items <- worry :: monkeys.(goal).items;
      );

      monkey.checks <- monkey.checks + List.length monkey.items;
      monkey.items <- [];
    );
  done;

  let (a, b) = Array.fold monkeys ~init:(0, 0) ~f:(fun (m1, m2) item ->
    if item.checks > m1 then (item.checks, m1)
    else if item.checks > m2 then (m1, item.checks)
    else (m1, m2))
  in

  a * b

let part1 = emulate ~count:20 ~calc_worry:(fun a b ~op ~old -> (op (operand a ~old) (operand b ~old)) / 3)

let part2 monkeys =
  let fac = Array.fold monkeys ~init:1 ~f:(fun a m -> a * m.test_divider) in

  emulate monkeys ~count:10_000 ~calc_worry:(fun a b ~op ~old -> (op (operand a ~old) (operand b ~old)) % fac)

let () =
  let filename = "day11/input.txt" in

  parse filename |> part1 |> printf "part1: %i\n";
  parse filename |> part2 |> printf "part2: %i\n";

