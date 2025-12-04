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

let find_all_accessible grid =
  grid
  |> Array.to_list
  |> List.mapi (fun row line ->
      String.to_seq line
      |> Seq.mapi (fun col _ -> (row, col))
      |> Seq.filter (fun (r, c) -> is_accessible grid r c)
      |> List.of_seq)
  |> List.flatten

module IntPair = struct
  type t = int * int
  let compare = compare
end

module PosSet = Set.Make(IntPair)

let remove_positions grid positions =
  let position_set =
    positions
    |> List.fold_left (fun acc pos -> PosSet.add pos acc) PosSet.empty
  in
  grid
  |> Array.mapi (fun row line ->
      String.mapi (fun col c ->
        if PosSet.mem (row, col) position_set then '.'
        else c) line)

let get_neighbors grid positions =
  positions
  |> List.map (fun (row, col) ->
      [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)]
      |> List.filter_map (fun (dr, dc) ->
          let nr, nc = row + dr, col + dc in
          match get_char grid nr nc with
          | Some '@' -> Some (nr, nc)
          | _ -> None))
  |> List.flatten
  |> List.sort_uniq compare

let rec remove_with_candidates grid candidates total_removed =
  let accessible =
    candidates
    |> List.filter (fun (r, c) -> is_accessible grid r c)
  in
  match accessible with
  | [] -> total_removed
  | _ ->
      let new_grid = remove_positions grid accessible in
      let new_candidates = get_neighbors new_grid accessible in
      remove_with_candidates new_grid new_candidates (total_removed + List.length accessible)

let solve filename =
  let grid = read_input filename |> parse_grid in
  let initial_candidates = find_all_accessible grid in
  remove_with_candidates grid initial_candidates 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
