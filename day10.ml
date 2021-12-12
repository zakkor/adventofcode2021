open Printf
open Base

let input_lines = Core.In_channel.read_lines "./day10.input"

let matching_closing_delim = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | _ -> raise Stdlib.Not_found

let illegal_scores, incomplete_scores =
  List.partition_map input_lines ~f:(fun l ->
      let stack = Stack.create () in
      match
        List.find (String.to_list l) ~f:(fun delim ->
            if String.contains "([{<" delim then
              let () = Stack.push stack delim in
              false
            else
              match Stack.pop stack with
              | Some expect_open_delim ->
                  not
                    (Char.equal delim
                       (matching_closing_delim expect_open_delim))
              | None -> true)
      with
      | Some delim ->
          let illegal_score =
            match delim with
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | '>' -> 25137
            | _ -> raise Stdlib.Not_found
          in
          First illegal_score
      | None ->
          let incomplete_score =
            if Stack.length stack > 0 then
              (* Not illegal, just incomplete *)
              Stack.fold stack ~init:0 ~f:(fun acc delim ->
                  let delim = matching_closing_delim delim in
                  (acc * 5)
                  +
                  match delim with
                  | ')' -> 1
                  | ']' -> 2
                  | '}' -> 3
                  | '>' -> 4
                  | _ -> raise Stdlib.Not_found)
            else 0
          in
          Second incomplete_score)

let sum_illegal_scores = List.fold illegal_scores ~init:0 ~f:( + )

let () = printf "part 1: %d\n" sum_illegal_scores

let middle_incomplete_score =
  List.filter incomplete_scores ~f:(fun s -> s > 0) |> List.sort ~compare
  |> fun l -> List.nth_exn l (List.length incomplete_scores / 2)

let () = printf "part 2: %d\n" middle_incomplete_score
