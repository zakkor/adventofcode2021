let binary_of_string s = Scanf.sscanf ("0b" ^ s) "%i" (fun n -> n)

let lines = Core.In_channel.read_lines "./day3.input"

let sum_columns nums =
  List.fold_left
    (fun acc line ->
      List.map2 ( + ) acc
        (List.map
           (fun c -> if c = '1' then 1 else 0)
           (Base.String.to_list line)))
    (List.init (List.hd nums |> String.length) (fun _ -> 0))
    nums

let oxygen_rating nums len =
  List.map
    (fun n -> if float_of_int n >= float_of_int len /. 2. then 1 else 0)
    nums

let co2_rating nums len =
  List.map
    (fun n -> if float_of_int n < float_of_int len /. 2. then 1 else 0)
    nums

let filter_rating rating nums pos =
  let criteria = List.nth rating pos in
  List.filter
    (fun line ->
      let chr = char_of_int (int_of_char '0' + criteria) in
      line.[pos] = chr)
    nums

let rec search fn nums pos =
  if List.length nums = 1 then nums
  else
    search fn
      (filter_rating (fn (sum_columns nums) (List.length nums)) nums pos)
      (pos + 1)

let () =
  Printf.printf "%d\n"
    (binary_of_string (List.hd (search oxygen_rating lines 0))
    * binary_of_string (List.hd (search co2_rating lines 0)))
