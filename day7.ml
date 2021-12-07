open Printf

let input_lines = Core.In_channel.read_lines "./day7.input"

let crab_positions =
  List.(hd input_lines |> String.split_on_char ',' |> map int_of_string)

let max_pos = List.fold_left max (List.hd crab_positions) crab_positions

let avail_positions = List.init max_pos (fun i -> i)

let smallest_cost heuristic =
  let costs =
    List.map
      (fun start ->
        let move_cost acc fin = acc + heuristic (abs (start - fin)) in
        List.fold_left move_cost 0 crab_positions)
      avail_positions
  in
  List.fold_left min (List.hd costs) costs

let sigma n = n * (n + 1) / 2

let () =
  printf "best move p1: %d\n" @@ smallest_cost (fun n -> n);
  printf "best move p2: %d\n" @@ smallest_cost sigma
