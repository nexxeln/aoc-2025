#use "../utils.ml";;

module IntMap = Map.Make(Int)

let find_start line =
  let rec aux i =
    if i >= String.length line then 0
    else if line.[i] = 'S' then i
    else aux (i + 1)
  in
  aux 0

let add_count col count map =
  let current = IntMap.find_opt col map |> Option.value ~default:0 in
  IntMap.add col (current + count) map

let solve filename =
  let lines = read_input filename in
  let grid = Array.of_list lines in
  let height = Array.length grid in
  let width = if height > 0 then String.length grid.(0) else 0 in
  let start_col = find_start grid.(0) in

  let rec simulate row timelines =
    if row >= height || IntMap.is_empty timelines then
      IntMap.fold (fun _ count acc -> acc + count) timelines 0
    else
      let new_timelines =
        IntMap.fold (fun col count acc ->
          if grid.(row).[col] = '^' then
            acc
            |> (fun m -> if col > 0 then add_count (col - 1) count m else m)
            |> (fun m -> if col < width - 1 then add_count (col + 1) count m else m)
          else
            add_count col count acc
        ) timelines IntMap.empty
      in
      simulate (row + 1) new_timelines
  in

  simulate 1 (IntMap.singleton start_col 1)

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
