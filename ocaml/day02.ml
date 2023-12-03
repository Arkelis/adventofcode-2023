open Stdio
open Base
open Common

let lines = input_lines "inputs/day02.txt"

let parse_line line =
  let regex =
    Re.seq
      [
        Re.str "Game ";
        Re.group (Re.rep1 Re.digit);
        Re.str ": ";
        Re.group (Re.rep Re.any);
      ]
    |> Re.compile
  in
  let result = Re.exec regex line in
  (Re.Group.get result 1 |> Int.of_string, Re.Group.get result 2)

let tuples_from_group group =
  let regex =
    Re.seq
      [ Re.group (Re.rep1 Re.digit); Re.str " "; Re.group (Re.rep1 Re.alpha) ]
    |> Re.compile
  and to_tuple group =
    (Re.Group.get group 1 |> Int.of_string, Re.Group.get group 2)
  in
  Re.all regex group |> List.map ~f:to_tuple

let is_impossible_subset = function
  | n, "red" when n > 12 -> true
  | n, "green" when n > 13 -> true
  | n, "blue" when n > 14 -> true
  | _ -> false

let possible groups =
    groups |> tuples_from_group |> List.exists ~f:is_impossible_subset |> not

let possible_id (id, groups) = if possible groups then id else 0

let part1 =
  lines |> List.map ~f:parse_line |> List.map ~f:possible_id
  |> List.reduce_exn ~f:( + )

let get_cube_power cubes =
  let rec find_minimums cubes reds blues greens =
    match cubes with
    | (new_reds, "red") :: tl when new_reds > reds ->
        find_minimums tl new_reds blues greens
    | (new_greens, "green") :: tl when new_greens > greens ->
        find_minimums tl reds blues new_greens
    | (new_blues, "blue") :: tl when new_blues > blues ->
        find_minimums tl reds new_blues greens
    | _ :: tl -> find_minimums tl reds blues greens
    | [] -> [ reds; blues; greens ]
  in
  find_minimums cubes 0 0 0 |> List.reduce_exn ~f:( * )

let power (_, group) = group |> tuples_from_group |> get_cube_power

let part2 =
  lines |> List.map ~f:parse_line |> List.map ~f:power
  |> List.reduce_exn ~f:( + )

let () = Out_channel.printf "Part 1: %d\nPart 2: %d\n" part1 part2
