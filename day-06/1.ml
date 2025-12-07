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
  let rows = List.init height (fun row ->
    String.concat "" (List.map (fun col -> String.make 1 col.[row]) cols)
  ) in
  match List.rev rows with
  | op_row :: number_rows ->
    let numbers = List.filter_map (fun s ->
      let s = String.trim s in
      if s = "" then None else Some (int_of_string s)
    ) (List.rev number_rows) in
    let op = if String.contains op_row '*' then ( * ) else (+) in
    List.fold_left op (if String.contains op_row '*' then 1 else 0) numbers
  | [] -> 0

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
