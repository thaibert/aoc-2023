open In_channel

let max_red = 12
let max_green = 13
let max_blue = 14

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

(************************************)

type draw =
  | Red of int
  | Green of int
  | Blue of int

type game = {
  id: int;
  draws: draw list;
}

let to_game s: game =
  let to_draw s: draw =
    let clean_regex = Str.regexp {|\(^[ ,;]+\)\|\([ ,;]+$\)|} in
    let clean = Str.global_replace clean_regex "" s in
    match Str.split (Str.regexp " ") clean with
    | amount :: color :: [] -> (
        let amount = int_of_string amount in
        match color with
        | "red" -> Red amount
        | "green" -> Green amount
        | "blue" -> Blue amount
        | _ -> assert false
    )
    | _ -> assert false
  in
  let extract_draws s: draw list =
    let rec chomp_draw regex start s: draw list =
      if not (Str.string_match regex s start)
      then [] 
      else
        let matched = Str.matched_string s in
        let match_end = Str.match_end () in
        [to_draw matched] @ chomp_draw regex match_end s
    in
    chomp_draw (Str.regexp {|[ ,;]*[a-zA-Z0-9 ]+[ ,;]*|}) 0 s
  in
  let get_id s: (int * string) =
    let regex = Str.regexp {|Game \([0-9]+\):|} in
    let _ = Str.string_match regex s 0 in
    (
      (Str.matched_group 1 s |> int_of_string),
      (Str.match_end () |> Str.string_after s)
    )
  in
  let id, rest = get_id s in
  {
    id = id;
    draws = extract_draws rest
  }

let solution1 lines: int =
  let is_possible (g: game): bool =
    let is_possible (d: draw): bool = match d with
      | Red n -> n <= max_red
      | Green n -> n <= max_green
      | Blue n -> n <= max_blue
    in
    g.draws
      |> List.map is_possible
      |> List.fold_left (&&) true
  in
  lines
    |> List.map to_game
    |> List.filter is_possible
    |> List.map (fun g -> g.id)
    |> List.fold_left (+) 0


(************************************)

type maxes = { red: int; blue: int; green: int }
let solution2 lines: int =
  let max_draws_in_game (game: game): maxes =
    (* TODO *)
    let reds = game.draws |> List.map (fun draw ->
      match draw with | Red x -> x | _ -> -1) in
    let greens = game.draws |> List.map (fun draw ->
      match draw with | Green x -> x | _ -> -1) in
    let blues = game.draws |> List.map (fun draw ->
      match draw with | Blue x -> x | _ -> -1) in
    let max =
      List.fold_left (fun acc curr -> if curr > acc then curr else acc) 0
    in
    {
      red = reds |> max;
      blue = blues |> max;
      green = greens |> max;
    }
  in
  let power maxes = maxes.red * maxes.green * maxes.blue in
  lines
    |> List.map to_game
    |> List.map max_draws_in_game
    |> List.map power
    |> List.fold_left (+) 0

(************************************)

let run1 () =
  let ichannel = open_in("resources/aoc02/input") in
  let out = solution1 (read_lines ichannel) in
  Printf.printf "02-01 Total: %d\n" out;
  close_in ichannel

let run2 () =
  let ichannel = open_in("resources/aoc02/input") in
  let out = solution2 (read_lines ichannel) in
  Printf.printf "02-02 Total: %d\n" out;
  close_in ichannel
