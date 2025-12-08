#use "../utils.ml";;

module IntSet = Set.Make(Int)

let find_start line =
  let rec aux i =
    if i >= String.length line then 0
    else if line.[i] = 'S' then i
    else aux (i + 1)
  in
  aux 0

let solve filename =
  let lines = read_input filename in
  let grid = Array.of_list lines in
  let height = Array.length grid in
  let width = if height > 0 then String.length grid.(0) else 0 in
  let start_col = find_start grid.(0) in

  let rec simulate row active_cols split_count =
    if row >= height || IntSet.is_empty active_cols then split_count
    else
      let (new_cols, new_splits) =
        IntSet.fold (fun col (cols, splits) ->
          if grid.(row).[col] = '^' then
            let cols' =
              cols
              |> (fun s -> if col > 0 then IntSet.add (col - 1) s else s)
              |> (fun s -> if col < width - 1 then IntSet.add (col + 1) s else s)
            in
            (cols', splits + 1)
          else
            (IntSet.add col cols, splits)
        ) active_cols (IntSet.empty, 0)
      in
      simulate (row + 1) new_cols (split_count + new_splits)
  in

  simulate 1 (IntSet.singleton start_col) 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
