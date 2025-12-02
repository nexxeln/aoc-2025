#use "../utils.ml";;

let is_invalid n =
  let s = string_of_int n in
  let len = String.length s in
  let rec try_pattern_len plen =
    if plen > len / 2 then false
    else if len mod plen = 0 then
      let pattern = String.sub s 0 plen in
      let rec check_repeats pos =
        if pos >= len then true
        else if String.sub s pos plen = pattern then check_repeats (pos + plen)
        else false
      in
      if check_repeats 0 then true
      else try_pattern_len (plen + 1)
    else try_pattern_len (plen + 1)
  in
  try_pattern_len 1

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
  Printf.printf "Part 2: %d\n" (solve input)
