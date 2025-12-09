#use "../utils.ml";;

let parse_line line =
  match String.split_on_char ',' line with
  | [x; y] -> (int_of_string x, int_of_string y)
  | _ -> failwith "Invalid input"

let parse_input lines =
  lines
  |> List.map parse_line
  |> Array.of_list

let build_sorted_edges arr =
  let n = Array.length arr in
  let rec collect i horiz vert =
    if i >= n then (horiz, vert)
    else
      let (x1, y1) = arr.(i) in
      let (x2, y2) = arr.((i + 1) mod n) in
      if y1 = y2 then
        collect (i + 1) ((y1, min x1 x2, max x1 x2) :: horiz) vert
      else
        collect (i + 1) horiz ((x1, min y1 y2, max y1 y2) :: vert)
  in
  let (horiz, vert) = collect 0 [] [] in
  let horiz_arr = horiz |> List.sort compare |> Array.of_list in
  let vert_arr = vert |> List.sort compare |> Array.of_list in
  (horiz_arr, vert_arr)

let lower_bound arr target extract =
  let rec search lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if extract arr.(mid) < target then search (mid + 1) hi
      else search lo mid
  in
  search 0 (Array.length arr)

let has_horiz_intersection horiz xmin xmax ymin ymax =
  let start = lower_bound horiz (ymin + 1) (fun (y, _, _) -> y) in
  let len = Array.length horiz in
  let rec check i =
    if i >= len then false
    else
      let (y, a, b) = horiz.(i) in
      if y >= ymax then false
      else if a < xmax && b > xmin then true
      else check (i + 1)
  in
  check start

let has_vert_intersection vert xmin xmax ymin ymax =
  let start = lower_bound vert (xmin + 1) (fun (x, _, _) -> x) in
  let len = Array.length vert in
  let rec check i =
    if i >= len then false
    else
      let (x, a, b) = vert.(i) in
      if x >= xmax then false
      else if a < ymax && b > ymin then true
      else check (i + 1)
  in
  check start

let point_inside arr (px, py) =
  let n = Array.length arr in
  let rec count i acc =
    if i >= n then acc mod 2 = 1
    else
      let (x1, y1) = arr.(i) in
      let (x2, y2) = arr.((i + 1) mod n) in
      if x1 = x2 then
        let ymin, ymax = min y1 y2, max y1 y2 in
        let crosses = py > ymin && py <= ymax && x1 > px in
        count (i + 1) (if crosses then acc + 1 else acc)
      else
        count (i + 1) acc
  in
  count 0 0

let is_valid_rect arr horiz vert (x1, y1) (x2, y2) =
  let xmin, xmax = min x1 x2, max x1 x2 in
  let ymin, ymax = min y1 y2, max y1 y2 in
  if has_horiz_intersection horiz xmin xmax ymin ymax then false
  else if has_vert_intersection vert xmin xmax ymin ymax then false
  else
    let cx, cy = (xmin + xmax) / 2, (ymin + ymax) / 2 in
    point_inside arr (cx, cy)

let area (x1, y1) (x2, y2) =
  (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let solve filename =
  let arr = read_input filename |> parse_input in
  let n = Array.length arr in
  let (horiz, vert) = build_sorted_edges arr in
  let rec check_pairs i j best =
    if i >= n then best
    else if j >= n then check_pairs (i + 1) (i + 2) best
    else
      let p1 = arr.(i) in
      let p2 = arr.(j) in
      let (x1, y1), (x2, y2) = p1, p2 in
      let best' =
        if x1 <> x2 && y1 <> y2 && is_valid_rect arr horiz vert p1 p2 then
          max best (area p1 p2)
        else
          best
      in
      check_pairs i (j + 1) best'
  in
  check_pairs 0 1 0

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
