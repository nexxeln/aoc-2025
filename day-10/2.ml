#use "../utils.ml";;

let gcd a b =
  let rec g a b = if b = 0 then a else g b (a mod b) in
  g (abs a) (abs b)

let make_rat n d =
  let g = gcd n d in
  let n, d = n / g, d / g in
  if d < 0 then (-n, -d) else (n, d)

let rat_add (n1,d1) (n2,d2) = make_rat (n1*d2 + n2*d1) (d1*d2)
let rat_sub (n1,d1) (n2,d2) = make_rat (n1*d2 - n2*d1) (d1*d2)
let rat_mul (n1,d1) (n2,d2) = make_rat (n1*n2) (d1*d2)
let rat_div (n1,d1) (n2,d2) = make_rat (n1*d2) (d1*n2)
let rat_neg (n,d) = (-n, d)
let rat_zero = (0, 1)
let rat_of_int n = (n, 1)
let rat_is_zero (n,_) = n = 0
let rat_is_int (_,d) = d = 1
let rat_to_int (n,d) = n / d
let rat_floor (n,d) = if n >= 0 then n / d else (n - d + 1) / d
let rat_lt (n1,d1) (n2,d2) = n1*d2 < n2*d1
let rat_gt (n1,d1) (n2,d2) = n1*d2 > n2*d1

let parse_line line =
  let brace_start = String.index line '{' in
  let brace_end = String.index line '}' in
  let joltage =
    String.sub line (brace_start + 1) (brace_end - brace_start - 1)
    |> String.split_on_char ','
    |> List.map (fun s -> int_of_string (String.trim s))
  in
  let n_counters = List.length joltage in
  let bracket_end = String.index line ']' in
  let buttons_section = String.sub line bracket_end (brace_start - bracket_end) in
  let rec extract_buttons s acc =
    match String.index_opt s '(' with
    | None -> List.rev acc
    | Some start ->
      let end_idx = String.index s ')' in
      let content = String.sub s (start + 1) (end_idx - start - 1) in
      let after = String.sub s (end_idx + 1) (String.length s - end_idx - 1) in
      let indices =
        String.split_on_char ',' content
        |> List.map (fun s -> int_of_string (String.trim s))
      in
      let button = List.init n_counters (fun i -> if List.mem i indices then 1 else 0) in
      extract_buttons after (button :: acc)
  in
  (extract_buttons buttons_section [], joltage)

let matrix_get mat r c = List.nth (List.nth mat r) c
let matrix_set_row mat r new_row = List.mapi (fun i row -> if i = r then new_row else row) mat
let matrix_swap_rows mat r1 r2 =
  let row1 = List.nth mat r1 in
  let row2 = List.nth mat r2 in
  List.mapi (fun i row -> if i = r1 then row2 else if i = r2 then row1 else row) mat

let row_scale row scale = List.map (fun x -> rat_div x scale) row
let row_sub row1 row2 factor = List.map2 (fun a b -> rat_sub a (rat_mul factor b)) row1 row2

let gaussian_elim mat n_buttons =
  let n_rows = List.length mat in
  let rec elim_col col pivot_row mat pivot_cols =
    if col >= n_buttons || pivot_row >= n_rows then (mat, pivot_cols)
    else
      let pivot_idx =
        List.mapi (fun i row -> (i, List.nth row col)) mat
        |> List.filteri (fun i _ -> i >= pivot_row)
        |> List.find_opt (fun (_, v) -> not (rat_is_zero v))
        |> Option.map fst
      in
      match pivot_idx with
      | None -> elim_col (col + 1) pivot_row mat pivot_cols
      | Some idx ->
        let mat = matrix_swap_rows mat pivot_row idx in
        let pivot_val = matrix_get mat pivot_row col in
        let scaled_row = row_scale (List.nth mat pivot_row) pivot_val in
        let mat = matrix_set_row mat pivot_row scaled_row in
        let mat = List.mapi (fun r row ->
          if r = pivot_row then row
          else
            let factor = List.nth row col in
            if rat_is_zero factor then row
            else row_sub row scaled_row factor
        ) mat in
        elim_col (col + 1) (pivot_row + 1) mat (col :: pivot_cols)
  in
  let (mat, pivot_cols) = elim_col 0 0 mat [] in
  (mat, List.rev pivot_cols)

let solve buttons target =
  let n_buttons = List.length buttons in

  let mat = List.mapi (fun r t ->
    List.mapi (fun _ btn -> rat_of_int (List.nth btn r)) buttons @ [rat_of_int t]
  ) target in

  let (mat, pivot_cols) = gaussian_elim mat n_buttons in

  let free_cols =
    List.init n_buttons Fun.id
    |> List.filter (fun c -> not (List.mem c pivot_cols))
  in
  let n_free = List.length free_cols in

  let pivot_exprs = List.mapi (fun i _ ->
    let row = List.nth mat i in
    let constant = List.nth row n_buttons in
    let coeffs = List.map (fun fc -> rat_neg (List.nth row fc)) free_cols in
    (constant, coeffs)
  ) pivot_cols in

  if n_free = 0 then
    List.fold_left (fun acc (const, _) -> acc + rat_to_int const) 0 pivot_exprs
  else
    let max_target = List.fold_left max 0 target in

    let eval_assignment assigned =
      let rec check exprs total =
        match exprs with
        | [] -> Some total
        | (constant, coeffs) :: rest ->
          let v = List.fold_left2 (fun acc c a -> rat_add acc (rat_mul c (rat_of_int a))) constant coeffs assigned in
          if not (rat_is_int v) || fst v < 0 then None
          else check rest (total + rat_to_int v)
      in
      check pivot_exprs (List.fold_left (+) 0 assigned)
    in

    let compute_upper assigned_so_far fi =
      List.fold_left (fun hi (constant, coeffs) ->
        let coef = List.nth coeffs fi in
        if not (rat_lt coef rat_zero) then hi
        else
          let prev = List.fold_left2 (fun acc c a -> rat_add acc (rat_mul c (rat_of_int a)))
            constant (List.filteri (fun j _ -> j < fi) coeffs) assigned_so_far in
          let future = List.fold_left (fun acc (j, c) ->
            if j > fi && rat_gt c rat_zero
            then rat_add acc (rat_mul c (rat_of_int max_target))
            else acc
          ) rat_zero (List.mapi (fun j c -> (j, c)) coeffs) in
          let bound = rat_floor (rat_div (rat_neg (rat_add prev future)) coef) in
          min hi (max 0 bound)
      ) max_target pivot_exprs
    in

    let rec search fi assigned best =
      let current_sum = List.fold_left (+) 0 assigned in
      if current_sum >= best then best
      else if fi = n_free then
        match eval_assignment (List.rev assigned) with
        | Some total when total < best -> total
        | _ -> best
      else
        let hi = compute_upper (List.rev assigned) fi in
        List.init (hi + 1) Fun.id
        |> List.fold_left (fun best v -> search (fi + 1) (v :: assigned) best) best
    in
    search 0 [] max_int

let solve_all filename =
  read_input filename
  |> List.map parse_line
  |> List.map (fun (b, t) -> solve b t)
  |> List.fold_left (+) 0

let () =
  let test_result = solve_all "test.txt" in
  Printf.printf "Test result: %d\n" test_result;
  let result = solve_all "input.txt" in
  Printf.printf "Result: %d\n" result
