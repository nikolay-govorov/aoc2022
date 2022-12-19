open Base
open Stdio

type lexem = BracketOpen | BracketClose | Delimiter | Digit of string

type packet =
  | PInt of int
  | PLst of packet list

let parse_line line =
  let lexems = String.fold line ~init:[] ~f:(fun lexs it ->
    match it with
    | '[' -> BracketOpen  :: lexs
    | ']' -> BracketClose :: lexs

    | ',' -> Delimiter    :: lexs

    | x when Char.is_digit x -> begin
      let dig = String.of_char it in

      match lexs with
      | Digit hd :: tl -> Digit (hd ^ dig) :: tl (* in digit *)
      | _ -> Digit dig :: lexs                   (* add new digit *)
    end

    | _ -> assert false
  )
  in

  let st = Stack.create () in

  List.rev lexems |> List.iter ~f:(fun lex ->
    let add item =
      match Stack.pop st with
      | Some PLst top -> Stack.push st (PLst (item :: top))
      | _ -> assert false
    in

    match lex with
    | Digit dig -> PInt (Int.of_string dig) |> add

    | BracketOpen  -> Stack.push st (PLst [])
    | BracketClose ->
        if Stack.length st > 1 then
          Stack.pop_exn st |> add

    | Delimiter    -> () (* ignore *)
  );

  assert (Stack.length st = 1);
  Stack.pop st |> Option.value_exn

let rec signal_compare l r =
  let open Continue_or_stop in

  let rec fold2_until q1 q2 ~f ~init =
    let callback ltl rtl return =
      match return with
      | Stop result -> result
      | Continue result -> fold2_until ltl rtl ~f ~init:result
    in

    match (q1, q2) with
    | ([], []) -> init

    | ([], b :: btl) -> f (None, Some(b)) |> callback [] btl
    | (a :: atl, []) -> f (Some(a), None) |> callback atl []
    | (a :: atl, b :: btl) -> f (Some(a), Some(b)) |> callback atl btl
  in

  match (l, r) with
  | (PLst a, PInt b) -> signal_compare (PLst a) (PLst [PInt b])
  | (PInt a, PLst b) -> signal_compare (PLst [PInt a]) (PLst b)

  | (PInt lq, PInt rq) -> Int.compare lq rq

  | (PLst lq, PLst rq) -> begin
    fold2_until (List.rev lq) (List.rev rq) ~init:0 ~f:(function
      | (Some a, Some b) -> begin
          match signal_compare a b with
          | 0 -> Continue(0)
          | x -> Stop(x)
      end

      | (None, _) -> Stop(-1)
      | (_, None) -> Stop(1)
    )
  end

let part1 lines =
  List.map lines ~f:parse_line
  |> List.groupi ~break:(fun i _ _ -> (i % 2 = 0))
  |> List.foldi ~init:0 ~f:(fun i acc [a; b] ->
    if signal_compare a b < 0 then acc + i + 1 else acc)

let part2 lines =
  let lines = "[[2]]" :: "[[6]]" :: lines in
  let sorted = List.map lines ~f:(fun line -> (line, parse_line line))
    |> List.sort ~compare:(fun (_, a) (_, b) -> signal_compare a b)
  in

  let (ai, bi) = List.foldi sorted ~init:(-1, -1) ~f:(fun i (ai, bi) (line, _) ->
    match line with
    | "[[2]]" -> (i + 1, bi)
    | "[[6]]" -> (ai, i + 1)
    | _ -> (ai, bi))
  in

  ai * bi

let () =
  let lines = In_channel.read_lines "day13/input.txt"
    |> List.filter ~f:(fun line -> String.equal line "" |> not)
  in

  part1 lines |> printf "part1: %i\n";
  part2 lines |> printf "part2: %i\n";

