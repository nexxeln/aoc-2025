#use "../utils.ml";;

let pad_lines lines =
  let max_len = List.fold_left max 0 (List.map String.length lines) in
  List.map (fun s -> s ^ String.make (max_len - String.length s) ' ') lines

let transpose lines =
  let grid = pad_lines lines |> Array.of_list in
  let height = Array.length grid in
  if height = 0 then []
  else
    let width = String.length grid.(0) in
    List.init width (fun col ->
      String.init height (fun row -> grid.(row).[col])
    )

let is_blank s = String.for_all ((=) ' ') s

let group_by_separator cols =
  let rec go acc current = function
    | [] -> List.rev (if current = [] then acc else List.rev current :: acc)
    | col :: rest when is_blank col ->
      if current = [] then go acc [] rest
      else go (List.rev current :: acc) [] rest
    | col :: rest -> go acc (col :: current) rest
  in
  go [] [] cols

let parse cols =
  let height = String.length (List.hd cols) in
  let parse_col col =
    let digits = String.sub col 0 (height - 1) in
    let digits = String.to_seq digits |> Seq.filter ((<>) ' ') |> String.of_seq in
    if digits = "" then None else Some (int_of_string digits)
  in
  let numbers = List.filter_map parse_col (List.rev cols) in
  let has_mult = List.exists (fun col -> col.[height - 1] = '*') cols in
  if has_mult
  then List.fold_left ( * ) 1 numbers
  else List.fold_left (+) 0 numbers

let solve filename =
  read_input filename
  |> List.filter (fun s -> String.length s > 0)
  |> transpose
  |> group_by_separator
  |> List.map parse
  |> List.fold_left (+) 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
