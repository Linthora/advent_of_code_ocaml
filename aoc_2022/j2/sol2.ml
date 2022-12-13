
let read_file_lines filename=
  let lines = ref [] in
  let fp = open_in filename in
  try
    while true; do
      lines := (input_line fp) :: !lines;
    done; !lines
  with End_of_file ->
    close_in fp;
    List.rev !lines;;

(* pb 1 *)

let translate x = (int_of_char x)

let eval_choice x =
  match x with
  | 'Y' -> 2
  | 'X' -> 1
  | 'Z' -> 3
  | _ -> 0

let eval_round r =
  let eval_game a b =
    match (a - b) with
    | -1 -> 6
    | 2 -> 6
    | 1 -> 0
    | -2 -> 0
    | _ -> 3 in
  match (String.split_on_char ' ' r) with
  | [a; b] -> (eval_choice (String.get b 0)) + (eval_game (translate (String.get a 0)) ((translate (String.get b 0)-23)))
  | _ -> 0

let rec eval_strategy l count =
  match l with
  | [] -> count
  | (x :: t) -> eval_strategy t ((eval_round x) + count)

(* pb 2 *)

let eval_game_reborn r =
  let eval_vict = function
    | "Y" -> 3
    | "Z" -> 6
    | _ -> 0 in
  let eval_roundito a b =
    match a, b with
    | _, 3 -> eval_choice (char_of_int (a+23))
    | 67, 6 -> eval_choice (char_of_int (a+21))
    | _, 6 -> eval_choice (char_of_int (a+24))
    | 65, _ -> eval_choice (char_of_int (a+25))
    | _, _ -> eval_choice (char_of_int (a+22)) in
  match (String.split_on_char ' ' r) with
  | [a; b] -> ((eval_vict b) + (eval_roundito (translate (String.get a 0)) (eval_vict b)))
  | _ -> 0

let rec eval_strategy_reborn l count =
  match l with
  | [] -> count
  | (x :: t) -> eval_strategy_reborn t ((eval_game_reborn x) + count)

(* Printing answer *)

let file = "input.txt"

let lines = read_file_lines file

let () = print_string "\n\nQuest1 answer: "

let () = print_int (eval_strategy lines 0)

let () = print_string "\n\nQuest2 answer: "

let () = print_int (eval_strategy_reborn lines 0)

let () = print_endline "\n\nend of prog"
