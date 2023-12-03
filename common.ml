open Stdio

let input_lines filename = In_channel.with_file ~f:In_channel.input_lines filename
