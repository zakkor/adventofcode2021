open Printf

let lines = Core.In_channel.read_lines "./day4.input"

let numbers =
  List.hd lines |> String.split_on_char ',' |> List.map int_of_string

type board = (int * bool) list list

let boards : board list =
  Base.List.drop lines 2
  |> List.filter (( <> ) "")
  |> Base.List.chunks_of ~length:5
  |> List.map
       (List.map (fun l ->
            Base.String.substr_replace_all l ~pattern:"  " ~with_:" "
            |> Base.String.strip |> String.split_on_char ' '
            |> List.map (fun s -> (int_of_string s, false))))

let draw_number (boards : board list) n : board list =
  List.map
    (List.map (List.map (fun (bn, marked) -> (bn, marked || n = bn))))
    boards

let is_bingo (b : board) : bool =
  let has_winning_row b =
    List.exists (fun row -> List.for_all (fun (_, marked) -> marked) row) b
  in
  has_winning_row b || has_winning_row (Base.List.transpose_exn b)

let rec last_winning_board numbers boards n : board * int =
  match List.find_opt is_bingo boards with
  (* If there is a winning board *)
  | Some b ->
      (* If it is the last board, return it *)
      if List.length boards = 1 then (b, n)
      (* Otherwise remove it from the board list and keep searching for winning boards *)
      else last_winning_board numbers (List.filter (fun bb -> bb != b) boards) n
  (* Keep drawing numbers and trying to find a winning board *)
  | None -> (
      match numbers with
      | [] -> (List.hd boards, 0)
      | n :: tl -> last_winning_board tl (draw_number boards n) n)

let winning_board, winning_number = last_winning_board numbers boards 0

let unmarked_sum =
  List.flatten winning_board
  |> List.map (fun (bn, marked) -> if not marked then bn else 0)
  |> List.fold_left ( + ) 0

let solution = unmarked_sum * winning_number

let () = printf "%d\n" solution

(* Printing utils *)
let print_board (b : board) =
  List.iter
    (fun r ->
      let () = printf "[ " in
      let () =
        List.iter
          (fun (bn, marked) ->
            if marked then printf "{%d} " bn else printf "%d " bn)
          r
      in
      printf "]\n")
    b
