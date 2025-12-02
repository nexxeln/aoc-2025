#use "../utils.ml";;

let parse_line line =
  let s = String.trim line in
  if s = "" then ('L', 0)
  else (s.[0], int_of_string (String.sub s 1 (String.length s - 1)))

let rotate pos dir dist =
  let new_pos = (if dir = 'L' then pos - dist else pos + dist) mod 100 in
  if new_pos < 0 then new_pos + 100 else new_pos

let count_zeros pos dir dist =
  if dist = 0 then 0 else
    let end_pos = if dir = 'L' then pos - dist else pos + dist in
    let rec loop zero_pos acc =
      let in_range = if dir = 'L' then zero_pos <= pos && zero_pos > end_pos
                     else zero_pos >= pos && zero_pos < end_pos in
      let continues = if dir = 'L' then zero_pos > end_pos else zero_pos < end_pos in
      if not continues then acc
      else loop (if dir = 'L' then zero_pos - 100 else zero_pos + 100)
                (if in_range then acc + 1 else acc)
    in
    loop 0 0

let solve lines =
  let rec count pos acc = function
    | [] -> acc
    | line :: rest ->
        let (dir, dist) = parse_line line in
        let zeros = count_zeros pos dir dist in
        count (rotate pos dir dist) (acc + zeros) rest
  in
  count 50 0 lines

let () =
  Printf.printf "Part 2: %d\n" (solve (read_input "input.txt"))
