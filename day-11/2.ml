#use "../utils.ml";;

module StringMap = Map.Make(String)

module Memo = Map.Make(struct
  type t = string * bool * bool
  let compare = compare
end)

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
  let rec dfs memo node visited_dac visited_fft =
    let visited_dac = visited_dac || node = "dac" in
    let visited_fft = visited_fft || node = "fft" in
    let key = (node, visited_dac, visited_fft) in
    match Memo.find_opt key memo with
    | Some count -> (memo, count)
    | None ->
      let (memo, count) =
        if node = "out" then
          (memo, if visited_dac && visited_fft then 1 else 0)
        else
          match StringMap.find_opt node graph with
          | None -> (memo, 0)
          | Some neighbors ->
            neighbors
            |> List.fold_left (fun (memo, total) n ->
                let (memo, c) = dfs memo n visited_dac visited_fft in
                (memo, total + c)
              ) (memo, 0)
      in
      (Memo.add key count memo, count)
  in
  let (_, count) = dfs Memo.empty "svr" false false in
  count

let solve filename =
  read_input filename
  |> parse_input
  |> count_paths

let () =
  let test_result = solve "test2.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
