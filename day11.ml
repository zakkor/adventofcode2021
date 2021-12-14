open Printf
open Base

let input_lines = Core.In_channel.read_lines "./day11.input"

let rows, cols =
  (List.length input_lines, List.hd_exn input_lines |> String.length)

let mat =
  let mat = Array.make_matrix ~dimx:rows ~dimy:cols 0 in
  List.iteri input_lines ~f:(fun i row ->
      List.iteri (String.to_list row) ~f:(fun j col ->
          mat.(i).(j) <- Char.get_digit_exn col));
  mat

let flash mat i j =
  for x = -1 to 1 do
    for y = -1 to 1 do
      let in_bounds =
        i + x >= 0 && i + x < cols && j + y >= 0 && j + y < rows
      in
      if in_bounds then mat.(i + x).(j + y) <- mat.(i + x).(j + y) + 1
    done
  done

let step_flashes mat =
  let flashed = Array.make_matrix ~dimx:rows ~dimy:cols false in
  Array.iteri mat ~f:(fun i row ->
      Array.iteri row ~f:(fun j _ -> mat.(i).(j) <- mat.(i).(j) + 1));
  let can_flash () =
    Array.existsi mat ~f:(fun i row ->
        Array.existsi row ~f:(fun j col -> col > 9 && not flashed.(i).(j)))
  in
  while can_flash () do
    Array.iteri mat ~f:(fun i row ->
        Array.iteri row ~f:(fun j col ->
            if col > 9 && not flashed.(i).(j) then (
              flash mat i j;
              flashed.(i).(j) <- true)))
  done;
  Array.iteri flashed ~f:(fun i row ->
      Array.iteri row ~f:(fun j flashed -> if flashed then mat.(i).(j) <- 0));

  let flash_count =
    Array.concat (Array.to_list flashed)
    |> Array.fold ~init:0 ~f:(fun acc flashed ->
           if flashed then acc + 1 else acc)
  in
  flash_count

let rec step mat acc accflashes =
  let flash_count = step_flashes mat in
  if acc = 1 then accflashes else (step mat (acc - 1)) (accflashes + flash_count)

let rec step_all mat acc =
  let flash_count = step_flashes mat in
  if flash_count = rows * cols then acc + 1 else step_all mat (acc + 1)

let print_mat mat =
  Array.iter mat ~f:(fun row ->
      let () =
        Array.iter row ~f:(fun c ->
            if c = 9 then printf "{%d}" c else printf "[%d]" c)
      in
      printf "\n")

let () =
  (* let () = printf "part 1: %d\n" (step mat 101 0) in *)
  let () = printf "part 2: %d\n" (step_all mat 0) in
  print_mat mat
