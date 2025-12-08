#use "../utils.ml";;

module DSU = struct
  module IntMap = Map.Make(Int)
  type t = int IntMap.t

  let empty = IntMap.empty

  let rec find parent i =
    match IntMap.find_opt i parent with
    | None -> (i, parent)
    | Some p when p = i -> (i, parent)
    | Some p ->
      let (root, parent') = find parent p in
      (root, IntMap.add i root parent')

  let union parent i j =
    let (ri, parent') = find parent i in
    let (rj, parent'') = find parent' j in
    if ri = rj then parent''
    else IntMap.add ri rj parent''
end

let parse_line line =
  match String.split_on_char ',' line with
  | [x; y; z] -> (int_of_string x, int_of_string y, int_of_string z)
  | _ -> failwith "Invalid input"

let distance (x1, y1, z1) (x2, y2, z2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let dz = z2 - z1 in
  dx * dx + dy * dy + dz * dz

let all_pairs points =
  let n = Array.length points in
  let rec aux acc i j =
    if i >= n then acc
    else if j >= n then aux acc (i + 1) (i + 2)
    else
      let d = distance points.(i) points.(j) in
      aux ((d, i, j) :: acc) i (j + 1)
  in
  aux [] 0 1

let solve filename connections =
  let lines = read_input filename in
  let points =
    lines
    |> List.filter (fun s -> String.length s > 0)
    |> List.map parse_line
    |> Array.of_list
  in
  let n = Array.length points in

  let pairs = all_pairs points in
  let sorted_pairs = List.sort compare pairs in

  let rec process_pairs dsu count pairs =
    if count >= connections then dsu
    else match pairs with
      | [] -> dsu
      | (_, i, j) :: rest ->
        let dsu' = DSU.union dsu i j in
        process_pairs dsu' (count + 1) rest
  in

  let final_dsu = process_pairs DSU.empty 0 sorted_pairs in

  let circuit_sizes =
    List.init n (fun i -> i)
    |> List.fold_left (fun (acc, dsu) i ->
        let (root, dsu') = DSU.find dsu i in
        let current = DSU.IntMap.find_opt root acc |> Option.value ~default:0 in
        (DSU.IntMap.add root (current + 1) acc, dsu')
      ) (DSU.IntMap.empty, final_dsu)
    |> fst
    |> DSU.IntMap.bindings
    |> List.map snd
    |> List.sort (fun a b -> compare b a)
  in

  match circuit_sizes with
  | a :: b :: c :: _ -> a * b * c
  | _ -> failwith "Not enough circuits"

let () =
  let test_result = solve "test.txt" 10 in
  Printf.printf "Test result: %d\n" test_result;

  let result = solve "input.txt" 1000 in
  Printf.printf "Result: %d\n" result
