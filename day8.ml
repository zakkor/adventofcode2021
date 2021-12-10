open Printf
module CharSet = Set.Make (Char)
module CharMap = Map.Make (Char)
module StringMap = Map.Make (String)

let input_lines = Core.In_channel.read_lines "./day8.input"

let rec product l =
  let rec aux ~acc l1 l2 =
    match (l1, l2) with
    | [], _ | _, [] -> acc
    | h1 :: t1, h2 :: t2 ->
        let acc = (h1 :: h2) :: acc in
        let acc = aux ~acc t1 l2 in
        aux ~acc [ h1 ] t2
  in
  match l with
  | [] -> []
  | [ l1 ] -> List.map (fun x -> [ x ]) l1
  | l1 :: tl ->
      let tail_product = product tl in
      aux ~acc:[] l1 tail_product

let values =
  List.map
    (fun l ->
      match String.split_on_char '|' l with
      | [ left; right ] ->
          ( String.trim left |> String.split_on_char ' ',
            String.trim right |> String.split_on_char ' ' )
      | _ -> raise Not_found)
    input_lines

let unique_digit_lengths s =
  let l = String.length s in
  (* Digits 1, 4, 7, 8 *)
  l = 2 || l = 4 || l = 3 || l = 7

let output_unique_digit_count =
  Base.List.fold_left values
    ~f:(fun acc (_, outputs) ->
      acc + List.length (List.filter unique_digit_lengths outputs))
    ~init:0

let () = printf "part 1: %d\n" output_unique_digit_count

type constraints = {
  t : char list list;
  tr : char list list;
  tl : char list list;
  c : char list list;
  br : char list list;
  bl : char list list;
  b : char list list;
}
[@@deriving show]

let output_sum =
  Base.List.fold_left values ~init:0 ~f:(fun acc (inputs, outputs) ->
      let unique_length_inputs = List.filter unique_digit_lengths inputs in
      let locked_constr =
        Base.List.fold_left unique_length_inputs
          ~init:{ t = []; tr = []; tl = []; c = []; br = []; bl = []; b = [] }
          ~f:(fun acc s ->
            let cl = [ Base.String.to_list s ] in
            match String.length s with
            | 2 -> { acc with tr = acc.tr @ cl; br = acc.br @ cl }
            | 3 ->
                { acc with t = acc.t @ cl; tr = acc.tr @ cl; br = acc.br @ cl }
            | 4 ->
                {
                  acc with
                  tl = acc.tl @ cl;
                  tr = acc.tr @ cl;
                  c = acc.c @ cl;
                  br = acc.br @ cl;
                }
            | 7 ->
                {
                  t = acc.t @ cl;
                  tl = acc.tl @ cl;
                  tr = acc.tr @ cl;
                  c = acc.c @ cl;
                  br = acc.br @ cl;
                  bl = acc.bl @ cl;
                  b = acc.b @ cl;
                }
            | _ -> acc)
      in
      let multiple_length_inputs =
        List.filter (fun i -> not (unique_digit_lengths i)) inputs
      in
      let maybe_constr =
        Base.List.map multiple_length_inputs ~f:(fun s ->
            let cl = [ Base.String.to_list s ] in
            match String.length s with
            | 5 ->
                [
                  { t = cl; tl = []; tr = cl; c = cl; br = []; bl = cl; b = cl };
                  { t = cl; tl = []; tr = cl; c = cl; br = cl; bl = []; b = cl };
                  { t = cl; tl = cl; tr = []; c = cl; br = cl; bl = []; b = cl };
                ]
            | 6 ->
                [
                  { t = cl; tl = cl; tr = cl; c = []; br = cl; bl = cl; b = cl };
                  { t = cl; tl = cl; tr = []; c = cl; br = cl; bl = cl; b = cl };
                  { t = cl; tl = cl; tr = cl; c = cl; br = cl; bl = []; b = cl };
                ]
            | _ -> [])
      in
      let all_possible_maybe_constraints = product maybe_constr in
      let try_constraints : constraints list =
        List.map
          (fun cl ->
            let combined_constraints =
              List.fold_left
                (fun acc c ->
                  {
                    t = acc.t @ c.t;
                    tl = acc.tl @ c.tl;
                    tr = acc.tr @ c.tr;
                    c = acc.c @ c.c;
                    br = acc.br @ c.br;
                    bl = acc.bl @ c.bl;
                    b = acc.b @ c.b;
                  })
                locked_constr cl
            in
            combined_constraints)
          all_possible_maybe_constraints
      in
      let merge_constraints c =
        let inter l =
          let sets = List.map CharSet.of_list l in
          [
            List.fold_left
              (fun acc s -> CharSet.inter acc s)
              (List.hd sets) sets
            |> CharSet.to_seq |> List.of_seq;
          ]
        in
        {
          t = inter c.t;
          tr = inter c.tr;
          tl = inter c.tl;
          c = inter c.c;
          br = inter c.br;
          bl = inter c.bl;
          b = inter c.b;
        }
      in
      let first_valid_constraint =
        List.map merge_constraints try_constraints
        |> List.find_map (fun c ->
               let p =
                 product
                   (List.flatten [ c.t; c.tr; c.tl; c.c; c.br; c.bl; c.b ])
                 |> List.filter (fun l ->
                        not (Base.List.contains_dup l ~compare))
               in
               if p |> List.length > 0 then Some (List.hd p) else None)
        |> Option.get
      in
      let wires =
        CharMap.add (List.nth first_valid_constraint 0) "t" CharMap.empty
      in
      let wires = CharMap.add (List.nth first_valid_constraint 1) "tr" wires in
      let wires = CharMap.add (List.nth first_valid_constraint 2) "tl" wires in
      let wires = CharMap.add (List.nth first_valid_constraint 3) "c" wires in
      let wires = CharMap.add (List.nth first_valid_constraint 4) "br" wires in
      let wires = CharMap.add (List.nth first_valid_constraint 5) "bl" wires in
      let wires = CharMap.add (List.nth first_valid_constraint 6) "b" wires in
      let digit_of_signal sg =
        let lit =
          List.fold_left
            (fun acc l -> StringMap.add (CharMap.find l wires) true acc)
            StringMap.empty sg
        in
        let t, tr, tl, c, br, bl, b =
          ( StringMap.find_opt "t" lit |> Option.is_some,
            StringMap.find_opt "tr" lit |> Option.is_some,
            StringMap.find_opt "tl" lit |> Option.is_some,
            StringMap.find_opt "c" lit |> Option.is_some,
            StringMap.find_opt "br" lit |> Option.is_some,
            StringMap.find_opt "bl" lit |> Option.is_some,
            StringMap.find_opt "b" lit |> Option.is_some )
        in
        match (t, tr, tl, c, br, bl, b) with
        | true, true, true, false, true, true, true -> '0'
        | false, true, false, false, true, false, false -> '1'
        | true, true, false, true, false, true, true -> '2'
        | true, true, false, true, true, false, true -> '3'
        | false, true, true, true, true, false, false -> '4'
        | true, false, true, true, true, false, true -> '5'
        | true, false, true, true, true, true, true -> '6'
        | true, true, false, false, true, false, false -> '7'
        | true, true, true, true, true, true, true -> '8'
        | true, true, true, true, true, false, true -> '9'
        | _ ->
            printf "%b %b %b %b %b %b %b" t tr tl c br bl b;
            raise Not_found
      in
      let output_number =
        List.map Base.String.to_list outputs
        |> List.map digit_of_signal |> Base.String.of_char_list |> int_of_string
      in
      acc + output_number)

let () = printf "part 2: %d\n" output_sum
