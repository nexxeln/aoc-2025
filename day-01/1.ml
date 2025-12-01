#use "../utils.ml";;

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
