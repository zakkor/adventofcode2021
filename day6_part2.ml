open Printf

let () = Printexc.record_backtrace true

let input_lines = Core.In_channel.read_lines "./day6.input"

let initial_fish =
  List.hd input_lines |> String.split_on_char ',' |> List.map int_of_string

let simulate days =
  let count_day day = Base.List.count initial_fish ~f:(fun f -> f = day) in
  let counts =
    ( count_day 0,
      count_day 1,
      count_day 2,
      count_day 3,
      count_day 4,
      count_day 5,
      0,
      0,
      0 )
  in
  let rec step acc counts =
    let d0, d1, d2, d3, d4, d5, d6, d7, d8 = counts in
    if acc = 0 then counts
    else step (acc - 1) (d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0)
  in
  let d0, d1, d2, d3, d4, d5, d6, d7, d8 = step days counts in
  d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8

let () = printf "immutable: %d\n" @@ simulate 256

let simulate_mut days =
  (* 0 to 8 days *)
  let counts = Array.make 9 0 in
  List.iter (fun f -> counts.(f) <- counts.(f) + 1) initial_fish;
  for _ = 0 to days - 1 do
    (* Make a copy of fish in day 0 *)
    let zeroc = counts.(0) in
    (* For days 1 through 8, move down one position *)
    for i = 1 to 8 do
      counts.(i - 1) <- counts.(i)
    done;
    (* Fish that were in day 0 reset to day 6 *)
    counts.(6) <- counts.(6) + zeroc;
    (* Day 0 amount of fish spawn as day 8 fish *)
    counts.(8) <- zeroc
  done;
  Array.fold_left ( + ) 0 counts

let () = printf "mutable: %d\n" @@ simulate_mut 256
