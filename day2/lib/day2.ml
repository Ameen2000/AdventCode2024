open Core

let num_extract str =
  let split = String.to_list str in
  let rec aux charl accum =
    match charl with
    | [] -> List.rev accum
    | h :: t when List.is_empty accum ->
      if Char.is_digit h then aux t (h :: accum) else aux t accum
    | h :: t when not (List.is_empty accum) ->
      if Char.is_digit h then aux t (h :: accum) else List.rev accum
    | _ -> [ '0' ]
  in
  let result = aux split [] in
  if List.is_empty result then -1 else String.of_list result |> Int.of_string
;;

module Game = struct
  type cube =
    { mutable red : int
    ; mutable green : int
    ; mutable blue : int
    }

  type game =
    { id : int
    ; cubes : cube list
    }

  let split str = String.split ~on:':' str
  let id str = split str |> List.hd_exn |> num_extract
  let pullouts str = split str |> List.last_exn |> String.split ~on:';'

  let extract_color str =
    let colors = [ "red"; "blue"; "green" ] in
    let words = String.split ~on:' ' str in
    List.find words ~f:(fun word -> List.mem colors word ~equal:String.( = ))
  ;;

  let hands str = String.split ~on:',' str

  let color_of_string init_cube str =
    let aux str =
      match extract_color str with
      | Some "red" -> init_cube.red <- num_extract str
      | Some "green" -> init_cube.green <- num_extract str
      | Some "blue" -> init_cube.blue <- num_extract str
      | _ -> ()
    in
    aux str;
    init_cube
  ;;

  let cube_of_hand str =
    let hand = hands str in
    List.fold_left ~init:{ red = 0; green = 0; blue = 0 } ~f:color_of_string hand
  ;;

  let game_of_str str =
    let game_id = id str in
    let game_pullouts = pullouts str in
    let game_cube = List.map ~f:cube_of_hand game_pullouts in
    { id = game_id; cubes = game_cube }
  ;;

  let condition = { red = 12; green = 13; blue = 14 }

  let compare cube1 cube2 =
    [ cube1.red >= cube2.red; cube1.green >= cube2.green; cube1.blue >= cube2.blue ]
  ;;

  let is_valid_pullout cube1 =
    List.fold_left ~init:true ~f:( && ) (compare condition cube1)
  ;;

  let is_valid_game game =
    List.map ~f:is_valid_pullout game.cubes |> List.fold_left ~init:true ~f:( && )
  ;;

  let max_colors game =
    let lst = game.cubes in
    let red cb = cb.red in
    let green cb = cb.green in
    let blue cb = cb.blue in
    let reds = List.map ~f:red lst in
    let greens = List.map ~f:green lst in
    let blues = List.map ~f:blue lst in
    let extract = List.fold_left ~init:0 ~f:max in
    extract reds, extract greens, extract blues
  ;;

  let power (reds, greens, blues) = reds * greens * blues
end
