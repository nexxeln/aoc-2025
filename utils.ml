let read_input filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let parse_line line =
  let s = String.trim line in
  if s = "" then ('L', 0)
  else (s.[0], int_of_string (String.sub s 1 (String.length s - 1)))

let rotate pos dir dist =
  let new_pos = (if dir = 'L' then pos - dist else pos + dist) mod 100 in
  if new_pos < 0 then new_pos + 100 else new_pos
