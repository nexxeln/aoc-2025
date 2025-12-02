#use "../utils.ml";;

let parse_line line =
  let s = String.trim line in
  if s = "" then ('L', 0)
  else (s.[0], int_of_string (String.sub s 1 (String.length s - 1)))

let rotate pos dir dist =
  let new_pos = (if dir = 'L' then pos - dist else pos + dist) mod 100 in
  if new_pos < 0 then new_pos + 100 else new_pos

let solve lines =
  let rec count pos acc = function
    | [] -> acc
    | line :: rest ->
        let (dir, dist) = parse_line line in
        let new_pos = rotate pos dir dist in
        count new_pos (if new_pos = 0 then acc + 1 else acc) rest
  in
  count 50 0 lines

let () =
  Printf.printf "Part 1: %d\n" (solve (read_input "input.txt"))
