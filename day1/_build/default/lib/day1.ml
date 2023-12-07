open Core

let getnums lstr =
  let rec aux lstr accum =
    match lstr with
    | [] -> accum
    | h :: t ->
        if Char.is_digit(h) then aux t (h :: accum)
        else aux t accum
  in
  List.rev @@ aux lstr []

let combine_digits lnums =
  (List.hd_exn lnums |> String.of_char) ^ (List.last_exn lnums |> String.of_char)


let starts_with ~prefix str =
  let len_prefix = String.length prefix in
  let len_str = String.length str in
  let mark = len_prefix <= len_str in
  match mark with
  | false -> false
  | true -> String.(=) (String.sub str ~pos:0 ~len:len_prefix) (prefix)


let match_named_int str =
  match str with
  | str when starts_with ~prefix:"zero" str -> 0
  | str when starts_with ~prefix:"one" str -> 1
  | str when starts_with ~prefix:"two" str -> 2
  | str when starts_with ~prefix:"three" str -> 3
  | str when starts_with ~prefix:"four" str -> 4
  | str when starts_with ~prefix:"five" str -> 5
  | str when starts_with ~prefix:"six" str -> 6
  | str when starts_with ~prefix:"seven" str -> 7
  | str when starts_with ~prefix:"eight" str -> 8
  | str when starts_with ~prefix:"nine" str -> 9
  | str when starts_with ~prefix:"0" str -> 0
  | str when starts_with ~prefix:"1" str -> 1
  | str when starts_with ~prefix:"2" str -> 2
  | str when starts_with ~prefix:"3" str -> 3
  | str when starts_with ~prefix:"4" str -> 4
  | str when starts_with ~prefix:"5" str -> 5
  | str when starts_with ~prefix:"6" str -> 6
  | str when starts_with ~prefix:"7" str -> 7
  | str when starts_with ~prefix:"8" str -> 8
  | str when starts_with ~prefix:"9" str -> 9
  | _ -> -1


let take_out sub_str str =
  let len_sub = String.length sub_str in
  let len_str = String.length str in
  if len_str < len_sub then
    str
  else if String.(=) (String.sub str ~pos:0 ~len:len_sub)  sub_str then
    String.sub str ~len:len_sub ~pos:(len_str - len_sub)
  else
    str



let new_get_nums str =
  let str_len = String.length str in
  let rec aux str index len accum =
    let sub_str = String.sub str ~pos:index ~len:len in
    match sub_str with
    | "" -> List.rev accum
    | sub_str -> aux str (index + 1) (len - 1) ((match_named_int sub_str) :: accum)
  in
  aux str 0 str_len []

let new_combine l =
  let first_digit = List.hd_exn l |> Int.to_string in
  let last_digit = List.last_exn l |> Int.to_string in
  Int.of_string (first_digit ^ last_digit)
