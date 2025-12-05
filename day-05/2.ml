#use "../utils.ml";;

let parse_range line =
  match String.split_on_char '-' line with
  | [a; b] -> Some (int_of_string a, int_of_string b)
  | _ -> None

let parse_input lines =
  lines |> List.filter_map parse_range

let merge_ranges ranges =
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) ranges in
  let rec merge acc = function
    | [] -> List.rev acc
    | (lo, hi) :: rest ->
      match acc with
      | (acc_lo, acc_hi) :: acc_rest when lo <= acc_hi + 1 ->
        merge ((acc_lo, max acc_hi hi) :: acc_rest) rest
      | _ -> merge ((lo, hi) :: acc) rest
  in
  merge [] sorted

let solve filename =
  read_input filename
  |> parse_input
  |> merge_ranges
  |> List.fold_left (fun acc (lo, hi) -> acc + hi - lo + 1) 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
