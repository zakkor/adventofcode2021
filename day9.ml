open Printf
open Base

let input_lines = Core.In_channel.read_lines "./day9.input"

let rows, cols =
  (List.length input_lines, List.hd_exn input_lines |> String.length)

let mat =
  let mat = Array.make_matrix ~dimx:rows ~dimy:cols 0 in
  List.iteri input_lines ~f:(fun i row ->
      List.iteri (String.to_list row) ~f:(fun j col ->
          mat.(i).(j) <- Char.get_digit_exn col));
  mat

let neighbours mat i j =
  let t = if i - 1 >= 0 then mat.(i - 1).(j) else 10 in
  let b = if i + 1 < rows then mat.(i + 1).(j) else 10 in
  let l = if j - 1 >= 0 then mat.(i).(j - 1) else 10 in
  let r = if j + 1 < cols then mat.(i).(j + 1) else 10 in
  (t, b, l, r)

let is_low_point mat v i j =
  (* Current cell must be lower than all neighbouring cells. *)
  let t, b, l, r = neighbours mat i j in
  v < t && v < b && v < l && v < r

let sum_low_points mat =
  Array.foldi mat ~init:0 ~f:(fun i acc row ->
      acc
      + Array.foldi row ~init:0 ~f:(fun j acc col ->
            acc + if is_low_point mat col i j then 1 + col else 0))

let () = printf "part 1: %d\n" (sum_low_points mat)

let visited = Array.make_matrix ~dimx:rows ~dimy:cols false

let rec find_basin mat i j =
  if not visited.(i).(j) then (
    visited.(i).(j) <- true;
    let t, b, l, r = neighbours mat i j in
    let v = mat.(i).(j) in
    let extend next i j =
      if next < 9 && next > v then find_basin mat i j else 0
    in
    1
    + extend t (i - 1) j
    + extend b (i + 1) j
    + extend l i (j - 1)
    + extend r i (j + 1))
  else 0

let basins mat =
  Array.mapi mat ~f:(fun i row ->
      Array.mapi row ~f:(fun j col ->
          if is_low_point mat col i j then find_basin mat i j else 0))

let mul_biggest_basins =
  basins mat |> Array.to_list |> Array.concat |> Array.to_list
  |> List.sort ~compare:(fun a b ->
         if a < b then 1 else if a > b then -1 else 0)
  |> (fun l -> List.take l 3)
  |> List.fold_left ~init:1 ~f:( * )

let () = printf "part 2: %d\n" mul_biggest_basins
