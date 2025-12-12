#use "../utils.ml";;

let parse_ints s =
  s
  |> String.map (fun c -> if c >= '0' && c <= '9' then c else ' ')
  |> String.split_on_char ' '
  |> List.filter_map int_of_string_opt

let solve filename =
  let lines = read_input filename in
  let shape_lines =
    lines
    |> List.filter (fun s ->
        String.length s > 0 && s.[0] <> ' ' && not (String.contains s 'x'))
  in
  let sizes =
    shape_lines
    |> String.concat "\n"
    |> String.split_on_char ':'
    |> List.map (String.fold_left (fun n c -> if c = '#' then n + 1 else n) 0)
    |> List.filter (( <> ) 0)
  in
  lines
  |> List.filter (fun s -> String.contains s 'x')
  |> List.filter (fun line ->
      match parse_ints line with
      | w :: h :: qtys ->
        List.fold_left2 (fun a q s -> a + q * s) 0 qtys sizes < w * h
      | _ -> false)
  |> List.length

let () =
  Printf.printf "Test: %d\n" (solve "test.txt");
  Printf.printf "Result: %d\n" (solve "input.txt")
