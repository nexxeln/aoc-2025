#use "../utils.ml";;

let is_invalid n =
  let s = string_of_int n in
  let len = String.length s in
  if len mod 2 = 0 then
    let half = len / 2 in
    String.sub s 0 half = String.sub s half half
  else false

let parse_range range =
  match String.split_on_char '-' range with
  | [a; b] -> (int_of_string a, int_of_string b)
  | _ -> (0, 0)

let count_invalid_in_range (start, end_val) =
  let rec loop n acc =
    if n > end_val then acc
    else if is_invalid n then loop (n + 1) (acc + n)
    else loop (n + 1) acc
  in
  loop start 0

let solve input =
  let ranges = String.split_on_char ',' (String.trim input) in
  List.fold_left (fun acc range ->
    acc + count_invalid_in_range (parse_range range)
  ) 0 ranges

let () =
  let input = String.concat "" (read_input "input.txt") in
  Printf.printf "Part 1: %d\n" (solve input)
