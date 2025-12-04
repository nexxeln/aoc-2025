#use "../utils.ml";;

let parse_grid lines =
  lines
  |> List.filter (fun line -> String.length line > 0)
  |> Array.of_list

let get_char grid row col =
  if row >= 0 && row < Array.length grid &&
     col >= 0 && col < String.length grid.(row)
  then Some grid.(row).[col]
  else None

let count_adjacent_rolls grid row col =
  [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)]
  |> List.filter_map (fun (dr, dc) -> get_char grid (row + dr) (col + dc))
  |> List.filter ((=) '@')
  |> List.length

let is_accessible grid row col =
  grid.(row).[col] = '@' && count_adjacent_rolls grid row col < 4

let count_accessible_rolls grid =
  grid
  |> Array.to_list
  |> List.mapi (fun row line ->
      String.to_seq line
      |> Seq.mapi (fun col _ -> if is_accessible grid row col then 1 else 0)
      |> Seq.fold_left (+) 0)
  |> List.fold_left (+) 0

let solve filename =
  read_input filename
  |> parse_grid
  |> count_accessible_rolls

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
