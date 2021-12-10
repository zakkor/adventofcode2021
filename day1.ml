open Printf

let numbers =
  Core.In_channel.read_lines "./day1.input" |> List.map int_of_string

let windows_of l ~length =
  let rec aux acc lw =
    if List.length acc < length then lw
    else
      match acc with
      | [] -> lw
      | head :: tail ->
          ([ head ] @ Base.List.take tail (length - 1)) :: aux tail lw
  in
  aux l []

let () =
  printf "%d\n"
    (windows_of ~length:2 numbers
    |> List.fold_left
         (fun acc l ->
           match l with
           | [ a; b ] -> acc + if a < b then 1 else 0
           | _ -> raise Not_found)
         0);
  printf "%d\n"
    (windows_of ~length:3 numbers
    |> List.map (fun l -> List.fold_left ( + ) 0 l)
    |> windows_of ~length:2
    |> List.fold_left
         (fun acc l ->
           match l with
           | [ a; b ] -> acc + if a < b then 1 else 0
           | _ -> raise Not_found)
         0)
