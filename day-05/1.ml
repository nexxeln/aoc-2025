#use "../utils.ml";;

let parse_range line =
  match String.split_on_char '-' line with
  | [a; b] -> Some (int_of_string a, int_of_string b)
  | _ -> None

let parse_input lines =
  let rec split_sections ranges ingredients = function
    | [] -> (List.rev ranges, List.rev ingredients)
    | "" :: rest -> split_sections ranges ingredients rest
    | line :: rest ->
      match parse_range line with
      | Some range -> split_sections (range :: ranges) ingredients rest
      | None -> split_sections ranges (int_of_string line :: ingredients) rest
  in
  split_sections [] [] lines

let in_range id (lo, hi) = id >= lo && id <= hi

let is_fresh ranges id = List.exists (in_range id) ranges

let solve filename =
  let ranges, ingredients = read_input filename |> parse_input in
  ingredients
  |> List.filter (is_fresh ranges)
  |> List.length

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
