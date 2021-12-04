let lines = Core.In_channel.read_lines "./day3.input"

let length = List.length lines

let sums =
  List.fold_left
    (fun acc line ->
      List.map2 ( + ) acc
        (List.map
           (fun c -> if c = '1' then 1 else 0)
           (Base.String.to_list line)))
    (List.init (List.hd lines |> String.length) (fun _ -> 0))
    lines

let gamma = List.map (fun n -> if n >= length / 2 then 1 else 0) sums

let epsilon = List.map (fun n -> if n < length / 2 then 1 else 0) sums

let binary_of_string s = Scanf.sscanf s "%i" (fun n -> n)

let binary_of_int_list xs =
  binary_of_string (List.fold_left (fun acc n -> acc ^ string_of_int n) "0b" xs)

let () =
  Printf.printf "%d\n" (binary_of_int_list gamma * binary_of_int_list epsilon)
