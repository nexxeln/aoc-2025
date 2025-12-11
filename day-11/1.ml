#use "../utils.ml";;

module StringMap = Map.Make(String)

let parse_line line =
  match String.split_on_char ':' line with
  | [src; dests] ->
    let dests = 
      dests 
      |> String.trim 
      |> String.split_on_char ' '
      |> List.filter (fun s -> s <> "")
    in
    Some (src, dests)
  | _ -> None

let parse_input lines =
  lines
  |> List.filter_map parse_line
  |> List.fold_left (fun acc (src, dests) -> StringMap.add src dests acc) StringMap.empty

let count_paths graph =
  let rec dfs memo node =
    match StringMap.find_opt node memo with
    | Some count -> (memo, count)
    | None ->
      let (memo, count) =
        if node = "out" then (memo, 1)
        else
          match StringMap.find_opt node graph with
          | None -> (memo, 0)
          | Some neighbors ->
            neighbors
            |> List.fold_left (fun (memo, total) n ->
                let (memo, c) = dfs memo n in
                (memo, total + c)
              ) (memo, 0)
      in
      (StringMap.add node count memo, count)
  in
  let (_, count) = dfs StringMap.empty "you" in
  count

let solve filename =
  read_input filename
  |> parse_input
  |> count_paths

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
