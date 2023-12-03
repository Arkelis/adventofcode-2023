open Stdio
open Base
open Option
open Common

let lines = input_lines "inputs/day01.txt"

let first_digit line =
  match line |> String.find ~f:Char.is_digit with
  | Some char -> char
  | None -> '0'

let last_digit line = line |> String.rev |> first_digit

let calibration line =
  let concat = String.of_char_list [ first_digit line; last_digit line ] in
  Int.of_string concat

let to_digit str =
  match str with
  | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> Int.of_string str
  | "one" -> 1
  | "two" -> 2
  | "three" -> 3
  | "four" -> 4
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> 0

let subs =
  [
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
  ]

let first_real_digit str =
  let rec search patterns str current_digit current_index =
    match patterns with
    | letter :: tl -> (
        match String.substr_index ~pattern:letter str with
        | Some index when index < current_index ->
            search tl str (to_digit letter) index
        | _ -> search tl str current_digit current_index)
    | _ -> current_digit
  in
  search subs str 0 @@ String.length str

let last_real_digit str =
  let rec search patterns str current_digit current_index =
    match patterns with
    | letter :: tl -> (
        match
          List.max_elt ~compare:Int.compare
          @@ String.substr_index_all ~may_overlap:false ~pattern:letter str
        with
        | Some index when index > current_index ->
            search tl str (to_digit letter) index
        | _ -> search tl str current_digit current_index)
    | _ -> current_digit
  in
  search subs str 0 @@ -1

let real_calibration line =
  [ first_real_digit line; last_real_digit line ]
  |> List.map ~f:Int.to_string |> String.concat |> Int.of_string

let part1 =
  let calibrations = List.map ~f:calibration lines in
  List.reduce_exn calibrations ~f:( + )

let part2 =
  let calibrations = List.map ~f:real_calibration lines in
  List.reduce_exn calibrations ~f:( + )

let () =
  [ part1; part2 ] |> List.map ~f:Int.to_string |> String.concat_lines
  |> Out_channel.print_endline
