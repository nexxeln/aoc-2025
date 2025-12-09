#use "../utils.ml";;

let parse_line line =
  match String.split_on_char ',' line with
  | [x; y] -> (int_of_string x, int_of_string y)
  | _ -> failwith "Invalid input"

let parse_input lines =
  lines
  |> List.map parse_line

let area (x1, y1) (x2, y2) =
  (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let max_rectangle points =
  let rec check_pairs best = function
    | [] -> best
    | p1 :: rest ->
      let best' = List.fold_left (fun acc p2 -> max acc (area p1 p2)) best rest in
      check_pairs best' rest
  in
  check_pairs 0 points

let solve filename =
  read_input filename
  |> parse_input
  |> max_rectangle

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
