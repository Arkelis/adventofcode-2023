open Base
open Stdio

let lines = Common.input_lines "inputs/day04.txt"

let parse_card line =
  let game_prefix_regex =
    Re.seq
      [ Re.str "Card"; Re.rep1 (Re.str " "); Re.rep1 Re.digit; Re.str ": " ]
    |> Re.compile
  in
  let line_without_game_prefix = Re.replace_string game_prefix_regex ~by:"" line
  and parse_numbers numbers_string =
    numbers_string
    |> Re.split (Re.rep1 (Re.str " ") |> Re.compile)
    |> List.map ~f:Int.of_string
    |> Set.of_list (module Int)
  in
  line_without_game_prefix |> String.split ~on:'|' |> List.map ~f:parse_numbers

let count_points card_numbers =
  match card_numbers with
  | [ winning; hand ] ->
      let count = Set.inter winning hand |> Set.length in
      if count > 0 then Int.pow 2 (count - 1) else 0
  | _ -> 0

let part1 =
  lines |> List.map ~f:parse_card |> List.map ~f:count_points
  |> List.reduce_exn ~f:( + )

let () = Out_channel.printf "Part 1: %d\nPart 2:\n" part1