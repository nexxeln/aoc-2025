#use "../utils.ml";;

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
