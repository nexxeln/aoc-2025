#use "../utils.ml";;

module IntMap = Map.Make(Int)

let parse_line line =
  match String.split_on_char ',' line with
  | [x; y; z] -> (int_of_string x, int_of_string y, int_of_string z)
  | _ -> failwith "Invalid input"

let distance (x1, y1, z1) (x2, y2, z2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let dz = z2 - z1 in
  dx * dx + dy * dy + dz * dz

let prim points =
  let n = Array.length points in

  let init_dists =
    List.init (n - 1) (fun i -> i + 1)
    |> List.fold_left (fun acc i ->
        IntMap.add i (distance points.(0) points.(i), 0) acc
      ) IntMap.empty
  in

  let rec loop dists max_edge =
    if IntMap.is_empty dists then max_edge
    else
      let (u, (d, from)) =
        IntMap.fold (fun node (d, from) (best_node, (best_d, best_from)) ->
          if d < best_d then (node, (d, from))
          else (best_node, (best_d, best_from))
        ) dists (-1, (max_int, -1))
      in

      let max_edge' =
        if d > fst max_edge then (d, (from, u))
        else max_edge
      in

      let dists' =
        IntMap.remove u dists
        |> IntMap.mapi (fun v (old_d, old_from) ->
            let new_d = distance points.(u) points.(v) in
            if new_d < old_d then (new_d, u)
            else (old_d, old_from))
      in

      loop dists' max_edge'
  in

  snd (loop init_dists (0, (0, 0)))

let solve filename =
  let lines = read_input filename in
  let points =
    lines
    |> List.filter (fun s -> String.length s > 0)
    |> List.map parse_line
    |> Array.of_list
  in
  let (i, j) = prim points in
  let (x1, _, _) = points.(i) in
  let (x2, _, _) = points.(j) in
  x1 * x2

let () =
  let test_result = solve "test.txt" in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" in
  Printf.printf "Result: %d\n" result
