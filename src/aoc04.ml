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

let rec implode s =
  match s with
  | x :: xs -> (string_of_char x) ^ implode xs
  | [] -> ""

let head xs = match xs with 
| x :: _ -> x
| [] -> 0
let butt xs = head (List.rev xs) 

type chomp_t =
  | Hit of { length: int; cursor_after: int }
  | Miss
let chomp_regex regex (s: string) index: chomp_t =
  let hit = Str.string_match regex s index in
  if not hit then
    Miss
  else
    let match_end = Str.match_end () in
    Hit {
      length = (match_end - index);
      cursor_after = match_end
    }

let printf_list format_str xs =
  let formatted = List.map (fun x -> Printf.sprintf format_str x) xs in
  Printf.printf "[";
  Printf.printf "%s" (String.concat ", " formatted);
  Printf.printf "]\n" 

let extract_numbers str: int list =
  let rec chomp_whitespace_separated regexp cursor =
    match chomp_regex regexp str cursor with
      | Hit { cursor_after; length } ->
          [String.sub str (cursor_after - length) length |> int_of_string]
            @ (chomp_whitespace_separated regexp cursor_after)
      | Miss -> (
        match chomp_regex (Str.regexp {|[ \n\r\x0c\t]+|}) str cursor with
          | Hit { cursor_after; _ } ->
              chomp_whitespace_separated regexp cursor_after
          | Miss -> []
        )
  in
  let regexp = Str.regexp {|[0-9]+|} in
  chomp_whitespace_separated regexp 0



(******)


type scratch_card_t = {
  card_index: int;
  own_numbers: int list;
  winning_numbers: int list
}

let own_numbers line: string =
  let _ = Str.string_match (Str.regexp {|^.*:|}) line 0 in
  let start = Str.match_end () in
  let _ = Str.string_match (Str.regexp {|[^|]+|}) line start in
  let numbers = Str.matched_string line in
  (* Printf.printf "own numbers:  %s\n" numbers; *)
  numbers
   
let winning_numbers line: string =
  let _ = Str.string_match (Str.regexp {|^.*||}) line 0 in
  let start = Str.match_end () in
  let _ = Str.string_match (Str.regexp {|.*|}) line start in
  let numbers = Str.matched_string line in
 (* Printf.printf "winning numbers:  %s\n" numbers; *)
  numbers

let line_to_card index line: scratch_card_t =
  {
    card_index = index;
    own_numbers = own_numbers line |> extract_numbers;
    winning_numbers = winning_numbers line |> extract_numbers;
  }

let rec union_of_lists list_a list_b =
  let contains target = List.exists (fun elem -> elem = target) in
  match list_a with
  | head :: tail when list_b |> contains head
      -> [head] @ union_of_lists tail list_b
  | _ :: tail -> union_of_lists tail list_b
  | [] -> []

(*********************************)
let solution1 (lines: string list): int =
  let value_of_card card =
    let matches = match card with | {own_numbers; winning_numbers} as card
      -> union_of_lists own_numbers winning_numbers |> List.length
    in
    match matches with
    | n when n <= 0 -> 0
    | n -> Int.shift_left 1 (n-1)
  in
  lines |> List.mapi line_to_card
        |> List.map value_of_card
        |> List.fold_left (+) 0

(************************************)

let solution2 (lines: string list): int =

  let original_cards = lines |> List.mapi line_to_card in
  0 


(************************************)

let run1 () =
  let ichannel = open_in("resources/aoc04/input") in
  let out = solution1 (read_lines ichannel) in
  Printf.printf "04-01 Total: %d\n" out;
  close_in ichannel

let run2 () =
  let ichannel = open_in("resources/aoc04/test") in
  let out = solution2 (read_lines ichannel) in
  Printf.printf "04-02 Total: %d\n" out;
  close_in ichannel
