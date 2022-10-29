open String
open List
open Map

let () = print_endline "Hello World\nEntering file:"

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

(* Question 1 *)

module Pos =
  struct
    type t = int * int
    let compare (i1,j1) (i2,j2) =
      match Stdlib.compare i1 i2 with
      | 0 -> Stdlib.compare j1 j2
      | c -> c
end


module SetPos = Set.Make(Pos)

let exploding_str s = List.init (String.length s) (String.get s);;

let rec moving_next s i j l =
  match (l, SetPos.add (i,j) s) with
  | '^' :: t, newS -> moving_next newS (i+1) j t
  | '>' :: t, newS -> moving_next newS i (j+1) t
  | '<' :: t, newS -> moving_next newS i (j-1) t
  | 'v' :: t, newS -> moving_next newS (i-1) j t
  | _, newS -> List.length (SetPos.elements newS);;

let how_many_houses lines = List.fold_left (+) 0 (List.map (moving_next (SetPos.empty) 0 0 ) (map (exploding_str) lines));;

(* Question 2*)

let trun_to_move (i1, j1, i2, j2) t =
  match t with
  | true -> (i1, j1, i2, j2)
  | _ -> (i2, j2, i1, j1);;

let rec moving_next_with_robot s i1 j1 i2 j2 l = (* coord : (i1, j1, i2, j2)*)
  match (l, SetPos.add (i1,j1) s) with
  | '^' :: t, newS -> moving_next_with_robot newS i2 j2 (i1+1) j1 t
  | '>' :: t, newS -> moving_next_with_robot newS i2 j2 i1 (j1+1) t
  | '<' :: t, newS -> moving_next_with_robot newS i2 j2 i1 (j1-1) t
  | 'v' :: t, newS -> moving_next_with_robot newS i2 j2 (i1-1) j1 t
  | _, newS -> List.length (SetPos.elements newS);;

let how_many_houses_with_robot lines = List.fold_left (+) 0 (List.map (moving_next_with_robot (SetPos.empty) 0 0 0 0 ) (map (exploding_str) lines));;





(* Printing answer *)

let file = "input.txt"

let lines = read_file_lines file

let () = print_string "\n\nQuest1 answer: "
let () = print_int (how_many_houses lines)

let () = print_string "\n\nQuest2 answer: "
let () = print_int (how_many_houses_with_robot lines)


let () = print_endline "\n\nend of prog"
