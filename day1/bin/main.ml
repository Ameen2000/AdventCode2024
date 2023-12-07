open Core
open Adventofcode

let file = "input1_day1.txt"
let file2 = "input2_day2"

let () =
  let inpt = In_channel.read_lines file in
  let inpt2 = In_channel.read_lines file2 in
  let mapfun x = String.to_list x |> Day1.getnums |> Day1.combine_digits |> Int.of_string in
  let mapfun2 l = Day1.new_get_nums l |> List.filter ~f:(fun x -> x > -1)  |> Day1.new_combine in
  let parse = List.map ~f:(mapfun) inpt in
  let parse2 = List.map ~f:(mapfun2) inpt2 in
  let result = List.fold ~init:0 ~f:( + ) parse in
  let result2 = List.fold ~init:0 ~f:( + ) parse2 in
  Printf.printf "Part1: %d\n" result;
  Printf.printf "Part2: %d\n" result2
