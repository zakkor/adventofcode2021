open Printf

let () = Printexc.record_backtrace true

let input_lines = Core.In_channel.read_lines "./day6.input"

let initial_fish =
  List.hd input_lines |> String.split_on_char ',' |> List.map int_of_string

let simulate days =
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

let () = printf "%d\n" @@ simulate 256
