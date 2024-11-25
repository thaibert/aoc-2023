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

type point_t = { line: int; column: int; }
type hitbox_t = { top_left: point_t; bottom_right: point_t; }
type serial_no_t = { number: int; hitbox: hitbox_t; }
let print_hitbox hitbox =
  let print_point point =
    Printf.printf "[%d; %d]" point.line point.column 
  in
  Printf.printf "{";
  print_point hitbox.top_left;
  print_point hitbox.bottom_right;
  Printf.printf "}"
let print_serial_no serial_no =
  Printf.printf "(%d: " serial_no.number;
  print_hitbox serial_no.hitbox;
  Printf.printf ")"


let rec line_to_hitboxes cursor line_index line: serial_no_t list =
  match chomp_regex (Str.regexp {|[0-9]+|}) line cursor with
    | Hit { cursor_after; length } ->
        let content =
          String.sub line (cursor_after - length) length |> int_of_string
        in
        [{
          number = content;
          hitbox = {
            top_left =     { line = line_index-1; column = cursor-1 };
            bottom_right = { line = line_index+1; column = cursor_after };
          }
        }] @ (line_to_hitboxes cursor_after line_index line)
    | Miss -> (
      match chomp_regex (Str.regexp {|[^0-9]+|}) line cursor with
        | Hit { cursor_after; _ } ->
            line_to_hitboxes cursor_after line_index line
        | Miss -> []
    ) 

(* TODO: ew, n^2 solution?! Could be linear by only using hitboxes in line +/- 1 *)
let inside_box serial_nos point: serial_no_t list =
  (*let rec any_true op = function
    | [] -> None
    | x :: xs -> if op x then Some x else any_true op xs
  in*)
  serial_nos |> List.fold_left (fun acc serial_no ->
    let is_inside =
         serial_no.hitbox.top_left.line   <= point.line
      && serial_no.hitbox.top_left.column <= point.column
      && serial_no.hitbox.bottom_right.line   >= point.line
      && serial_no.hitbox.bottom_right.column >= point.column
    in
    if is_inside then
      acc @ [serial_no]
    else
      acc
  ) []

(*********************************)
let solution1 (lines: string list) =
  let serial_nos = lines |> List.mapi (line_to_hitboxes 0) |> List.flatten in
  lines
    |> List.mapi (fun line_index line ->
        line |> explode |> List.mapi (fun column c -> 
          match c with
          | c when '0' <= c && c <= '9' -> 0
          | '.' -> 0
          | _ -> inside_box serial_nos { line = line_index; column }
                    |> List.map (fun x -> x.number)
                    |> List.fold_left (+) 0
        )
    )
    |> List.flatten
    |> List.fold_left (+) 0

  (*
  (* Diagnostic printing *)
  let print_list xs =
    List.iter print_serial_no xs;
    Printf.printf "\n"
  in
  lines
    |> List.mapi (line_to_hitboxes 0)
    |> List.iter print_list;

  Printf.printf "%b\n" (inside_box (lines |> List.mapi (line_to_hitboxes 0) |> List.hd) { line = 4; column = 5 })
  *)

(************************************)

let solution2 (lines: string list): int =
  let serial_nos = lines |> List.mapi (line_to_hitboxes 0) |> List.flatten in
  lines
    |> List.mapi (fun line_index line ->
        line |> explode |> List.mapi (fun column c -> 
          match c with
          | c when '0' <= c && c <= '9' -> 0
          | '.' -> 0
          | '*' -> (
              match inside_box serial_nos { line = line_index; column } with
              | a :: b :: [] -> a.number * b.number
              | _ -> 0
          )
          | _ -> 0
        )
    )
    |> List.flatten
    |> List.fold_left (+) 0

(************************************)

let run1 () =
  let ichannel = open_in("resources/aoc03/input") in
  let out = solution1 (read_lines ichannel) in
  Printf.printf "03-01 Total: %d\n" out;
  close_in ichannel

let run2 () =
  let ichannel = open_in("resources/aoc03/input") in
  let out = solution2 (read_lines ichannel) in
  Printf.printf "03-02 Total: %d\n" out;
  close_in ichannel
