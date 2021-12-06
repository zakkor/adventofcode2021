open Printf

let () = Printexc.record_backtrace true

let input_lines = Core.In_channel.read_lines "./day6.input"

let initial_fish =
  List.hd input_lines |> String.split_on_char ',' |> List.map int_of_string

let step ls =
  let rec aux acc = function
    | [] -> acc
    | f :: tail -> (
        match f - 1 with
        | -1 -> aux (6 :: 8 :: acc) tail
        | o -> aux (o :: acc) tail)
  in
  aux [] ls

let rec simulate fishes days =
  if days = 0 then fishes else simulate (step fishes) (days - 1)

let total_fish days = simulate initial_fish days |> List.length

let () = printf "%d\n" @@ total_fish 80
