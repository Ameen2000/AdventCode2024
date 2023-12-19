open Day2
open Core

let file = "part1.txt"

let () =
  let inpt = In_channel.read_lines file in
  let parsed = List.map ~f:Game.game_of_str inpt in
  let filtered = List.filter ~f:Game.is_valid_game parsed in
  let powers = List.map ~f:Game.max_colors parsed |> List.map ~f:Game.power in
  let id_of_game game = game.Game.id in
  let ids = List.map ~f:id_of_game filtered in
  let result = List.fold_left ~init:0 ~f:( + ) ids in
  let result2 = List.fold_left ~init:0 ~f:( + ) powers in
  Printf.printf "Answer to part 1: %d\n" result;
  Printf.printf "Answer to part 2: %d\n" result2
;;
