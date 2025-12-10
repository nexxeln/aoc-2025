#use "../utils.ml";;

let gf2_add a b = (a + b) mod 2
let gf2_sub = gf2_add
let gf2_mul a b = a * b
let gf2_is_zero a = a = 0

let parse_line line =
  let bracket_start = String.index line '[' in
  let bracket_end = String.index line ']' in
  let target =
    String.sub line (bracket_start + 1) (bracket_end - bracket_start - 1)
    |> String.to_seq
    |> List.of_seq
    |> List.map (fun c -> if c = '#' then 1 else 0)
  in
  let n = List.length target in
  let brace_pos = String.index line '{' in
  let buttons_section = String.sub line bracket_end (brace_pos - bracket_end) in
  let rec extract_buttons s acc =
    match String.index_opt s '(' with
    | None -> List.rev acc
    | Some start ->
      let end_idx = String.index s ')' in
      let content = String.sub s (start + 1) (end_idx - start - 1) in
      let after = String.sub s (end_idx + 1) (String.length s - end_idx - 1) in
      let indices = String.split_on_char ',' content |> List.map (fun s -> int_of_string (String.trim s)) in
      let button = List.init n (fun i -> if List.mem i indices then 1 else 0) in
      extract_buttons after (button :: acc)
  in
  (target, extract_buttons buttons_section [])

let matrix_swap_rows mat r1 r2 =
  List.mapi (fun i row -> if i = r1 then List.nth mat r2 else if i = r2 then List.nth mat r1 else row) mat

let row_sub_gf2 row1 row2 = List.map2 gf2_sub row1 row2

let gaussian_elim_gf2 mat n_buttons =
  let n_rows = List.length mat in
  let rec elim_col col pivot_row mat pivot_cols =
    if col >= n_buttons || pivot_row >= n_rows then (mat, pivot_cols)
    else
      let pivot_idx =
        List.mapi (fun i row -> (i, List.nth row col)) mat
        |> List.filteri (fun i _ -> i >= pivot_row)
        |> List.find_opt (fun (_, v) -> not (gf2_is_zero v))
        |> Option.map fst
      in
      match pivot_idx with
      | None -> elim_col (col + 1) pivot_row mat pivot_cols
      | Some idx ->
        let mat = matrix_swap_rows mat pivot_row idx in
        let pivot_row_data = List.nth mat pivot_row in
        let mat = List.mapi (fun r row ->
          if r = pivot_row then row
          else if gf2_is_zero (List.nth row col) then row
          else row_sub_gf2 row pivot_row_data
        ) mat in
        elim_col (col + 1) (pivot_row + 1) mat (col :: pivot_cols)
  in
  let (mat, pivot_cols) = elim_col 0 0 mat [] in
  (mat, List.rev pivot_cols)

let solve_machine (target, buttons) =
  let n_buttons = List.length buttons in

  let mat = List.mapi (fun r t ->
    List.map (fun btn -> List.nth btn r) buttons @ [t]
  ) target in

  let (mat, pivot_cols) = gaussian_elim_gf2 mat n_buttons in

  let free_cols =
    List.init n_buttons Fun.id
    |> List.filter (fun c -> not (List.mem c pivot_cols))
  in
  let n_free = List.length free_cols in

  let pivot_exprs = List.mapi (fun i _ ->
    let row = List.nth mat i in
    let constant = List.nth row n_buttons in
    let coeffs = List.map (fun fc -> List.nth row fc) free_cols in
    (constant, coeffs)
  ) pivot_cols in

  let rec search assignment_idx best =
    if assignment_idx >= (1 lsl n_free) then best
    else
      let assigned = List.mapi (fun i _ -> (assignment_idx lsr i) land 1) free_cols in
      let free_sum = List.fold_left (+) 0 assigned in
      if free_sum >= best then search (assignment_idx + 1) best
      else
        let pivot_sum = List.fold_left (fun acc (constant, coeffs) ->
          let v = List.fold_left2 (fun a c x -> gf2_add a (gf2_mul c x)) constant coeffs assigned in
          acc + v
        ) 0 pivot_exprs in
        let total = free_sum + pivot_sum in
        search (assignment_idx + 1) (min best total)
  in
  search 0 max_int

let solve filename =
  read_input filename
  |> List.map parse_line
  |> List.map solve_machine
  |> List.fold_left (+) 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;
  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
