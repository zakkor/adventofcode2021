open Printf

let input_lines = Core.In_channel.read_lines "./day5.input"

type point = { x : int; y : int }

type line = { start : point; fin : point }

let parse_point s =
  match String.split_on_char ',' s with
  | [ x; y ] -> { x = int_of_string x; y = int_of_string y }
  | _ -> raise (Invalid_argument s)

let parse_line s =
  match Str.split (Str.regexp " -> ") s with
  | [ start; fin ] -> { start = parse_point start; fin = parse_point fin }
  | _ -> raise (Invalid_argument s)

let parse_lines lines : line list = List.map parse_line lines

let lines = parse_lines input_lines

let map_size dim =
  let max_dim acc l =
    if dim l.start > acc then dim l.start
    else if dim l.fin > acc then dim l.fin
    else acc
  in
  1 + List.fold_left max_dim ~-1 lines

let map_width, map_height = (map_size (fun p -> p.x), map_size (fun p -> p.y))

let decompose (line : line) : point list =
  (* Range forwards and backwards *)
  let ( -- ) a b =
    let comp = if a < b then ( < ) else ( > ) in
    let op = if a < b then ( - ) else ( + ) in
    let rec aux n acc = if comp n a then acc else aux (op n 1) (n :: acc) in
    aux b []
  in
  (* If `a` is shorter than `b`, make `a` as long as `b` by appending its first element until the length matches *)
  let pad_right a b =
    let open List in
    if length a < length b then
      append a (init (length b - length a) (fun _ -> hd a))
    else a
  in
  let xl = line.start.x -- line.fin.x in
  let yl = line.start.y -- line.fin.y in
  let xl = pad_right xl yl in
  let yl = pad_right yl xl in
  let point_of_pair (x, y) = { x; y } in
  Base.List.zip_exn xl yl |> List.map point_of_pair

let draw_line (map : int array array) (line : line) =
  let points = decompose line in
  points |> List.iter (fun { x; y } -> map.(x).(y) <- map.(x).(y) + 1)

let filter_straight =
  List.filter (fun l -> l.start.x = l.fin.x || l.start.y = l.fin.y)

let count_overlapping_lines map =
  let flatten = Base.Array.concat_map ~f:(fun l -> l) in
  flatten map |> Array.fold_left (fun acc n -> acc + if n > 1 then 1 else 0) 0

(* Draw straight lines only *)
let map = Array.make_matrix map_width map_height 0

let () = filter_straight lines |> List.iter (draw_line map)

(* Part 1: *)
let () = printf "straight overlapping: %d\n" (count_overlapping_lines map)

(* Draw all lines *)
let map = Array.make_matrix map_width map_height 0

let () = lines |> List.iter (draw_line map)

(* Part 2: *)
let () = printf "all overlapping: %d\n" (count_overlapping_lines map)

let print_map (map : int list list) =
  List.iter
    (fun r ->
      let () =
        List.iter (fun c -> if c > 0 then printf "%d" c else printf ".") r
      in
      printf "\n")
    map
