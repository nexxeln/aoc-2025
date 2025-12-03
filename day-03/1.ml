#use "../utils.ml";;

let max_joltage bank batteries =
  let len = String.length bank in
  let rec pick start picked remaining =
    if remaining = 0 then picked
    else
      let max_idx = len - remaining in
      let rec find_max i best_idx =
        if i > max_idx then best_idx
        else if bank.[i] > bank.[best_idx] then
          find_max (i + 1) i
        else
          find_max (i + 1) best_idx
      in
      let chosen_idx = find_max start start in
      pick (chosen_idx + 1) (picked ^ String.make 1 bank.[chosen_idx]) (remaining - 1)
  in
  int_of_string (pick 0 "" batteries)

let solve filename =
  read_input filename
  |> List.filter (fun line -> String.length line > 0)
  |> List.map (fun bank -> max_joltage bank 2)
  |> List.fold_left (+) 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
