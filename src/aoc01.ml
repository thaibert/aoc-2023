open In_channel

let rec read_lines ic =
  match input_line ic with
  | Some line -> line :: read_lines ic
  | None -> []

let explode s =
  (* https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml *)
  let rec expl i acc =
    if i < 0 then acc else
      expl (i - 1) (s.[i] :: acc) in
  expl (String.length s - 1) []

  let string_of_char c = String.make 1 c
(*
let rec implode s =
  match s with
  | x :: xs -> (string_of_char x) ^ implode xs
  | [] -> ""
*)

let head xs = match xs with 
| x :: _ -> x
| [] -> 0
let butt xs = head (List.rev xs) 

(************************************)

let solution1 lines =
  let lex s =
    let rec chomp (chars: char list) = match chars with
    | '1' :: cs -> [1] @ chomp cs
    | '2' :: cs -> [2] @ chomp cs
    | '3' :: cs -> [3] @ chomp cs
    | '4' :: cs -> [4] @ chomp cs
    | '5' :: cs -> [5] @ chomp cs
    | '6' :: cs -> [6] @ chomp cs
    | '7' :: cs -> [7] @ chomp cs
    | '8' :: cs -> [8] @ chomp cs
    | '9' :: cs -> [9] @ chomp cs
    | _ :: cs -> chomp cs
    | _ -> []
    in chomp (explode s)
  in
  lines
  |> List.map lex
  |> List.map (fun (numbers: int list) ->
      (head numbers |> string_of_int) ^ (butt numbers |> string_of_int)
  )
  |> List.map int_of_string

(************************************)

let solution2 lines =
  let lex s = 
    let rec chomp (chars: char list) = match chars with
    | 'o'::('n'::'e'::_ as cs)           -> [1] @ chomp cs
    | 't'::('w'::'o'::_ as cs)           -> [2] @ chomp cs 
    | 't'::('h'::'r'::'e'::'e'::_ as cs) -> [3] @ chomp cs
    | 'f'::('o'::'u'::'r'::_ as cs)      -> [4] @ chomp cs
    | 'f'::('i'::'v'::'e'::_ as cs)      -> [5] @ chomp cs
    | 's'::('i'::'x'::_ as cs)           -> [6] @ chomp cs
    | 's'::('e'::'v'::'e'::'n'::_ as cs) -> [7] @ chomp cs
    | 'e'::('i'::'g'::'h'::'t'::_ as cs) -> [8] @ chomp cs
    | 'n'::('i'::'n'::'e'::_ as cs)      -> [9] @ chomp cs
    | '1' :: cs -> [1] @ chomp cs
    | '2' :: cs -> [2] @ chomp cs
    | '3' :: cs -> [3] @ chomp cs
    | '4' :: cs -> [4] @ chomp cs
    | '5' :: cs -> [5] @ chomp cs
    | '6' :: cs -> [6] @ chomp cs
    | '7' :: cs -> [7] @ chomp cs
    | '8' :: cs -> [8] @ chomp cs
    | '9' :: cs -> [9] @ chomp cs
    (* zero? *)
    | _ :: cs -> chomp cs
    | _ -> []
    in
    chomp (explode s)
  in
  lines
  |> List.map lex
  |> List.map (fun (numbers: int list) ->
      (head numbers |> string_of_int) ^ (butt numbers |> string_of_int)
  )
  |> List.map int_of_string

(************************************)

let run1 () =
  let ichannel = open_in("resources/aoc01/1.in") in
  let out = solution1 (read_lines ichannel) in
  (* Printf.printf "%s\n" (output |> List.map string_of_int |> String.concat "\n"); *)
  Printf.printf "01-01 Total: %d\n" (out |> List.fold_left (+) 0);
  close_in ichannel

let run2 () =
  let ichannel = open_in("resources/aoc01/2.in") in
  let out = solution2 (read_lines ichannel) in
  (* Printf.printf "%s\n" (out |> List.map string_of_int |> String.concat "\n"); *)
  Printf.printf "01-02 Total: %d\n" (out |> List.fold_left (+) 0);
  close_in ichannel

