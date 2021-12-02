type instr = Forward of int | Up of int | Down of int

let instr_of_string line =
  match String.split_on_char ' ' line with
  | [ "forward"; n ] -> Forward (int_of_string n)
  | [ "up"; n ] -> Up (int_of_string n)
  | [ "down"; n ] -> Down (int_of_string n)
  | _ -> raise Not_found

let lines = Core.In_channel.read_lines "./day2.input"

let instrs = List.map instr_of_string lines

let move (x, z) = function
  | Forward n -> (x + n, z)
  | Up n -> (x, z - n)
  | Down n -> (x, z + n)

let x, z = List.fold_left move (0, 0) instrs

let () = Printf.printf "%d" (x * z)
