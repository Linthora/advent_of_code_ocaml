open String
open List
open Str
open Map


let () = print_endline "Hello World\nDay5\nEntering file:"

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

let exploding_str s = List.init (String.length s) (String.get s);;

let vowels = ['a'; 'e'; 'i'; 'o'; 'u';]

let rec containsList a l =
  match l with
  | [] -> false
  | x :: _ when x == a -> true
  | _ :: xs -> containsList a xs ;;

(*
let contained a l = List.fold_left (||) false (map (a (=) ) l);;
*)
let rec contains_nb nb count chars l =
  match l with
  | _ when count >= nb -> true
  | [] -> false
  | x :: xs when (containsList x chars) -> contains_nb nb (count+1) chars xs
  | x :: xs -> contains_nb nb count chars xs;;

(*
let vowels3 = Str.regexp "*[aeiou].*[aeiou].*[aeiou].*"

//didn't work for some reason...
let double_letter = Str.regexp {|\(.\)\1|}
*)
let rec double_char l =
  match l with
  | c1 :: c2 :: t when c1 = c2 -> true
  | c :: t -> double_char t
  | [] -> false;;


let forbidden_combinaison = ["ab";"cd" ; "pq"; "xy";]

let rec inList c1 c2 l =
  match ((String.make 1 c1) ^ (String.make 1 c2)) , l with
  | _, [] -> false
  | x, h1 :: _ when x = h1 -> true
  | _, _ :: t -> inList c1 c2 t;;

let rec no_forbidden_doubles forbs l =
  match l with
  | [] -> true
  | h1 :: h2 :: _ when (inList h1 h2 forbs) -> false
  | h :: t -> no_forbidden_doubles forbs t;;

(*
let forbidden = Str.regexp {|.*\(\(ab\)\|\(cd\)\|\(pq\)\|\(xy\)\).*|}

let nice_sentence ok1 ko sentence = (Str.string_match ok1 sentence 0)
                                        && (double_char (exploding_str sentence))
                                        && (not (Str.string_match ko sentence 0));;
*)


let nice_sentence sentence =
  let exploded_sentence = exploding_str sentence in
  (contains_nb 3 0 vowels exploded_sentence) && (double_char exploded_sentence) && (no_forbidden_doubles forbidden_combinaison exploded_sentence);;

let rec counting_true l =
  match l with
  | true :: t -> 1 + counting_true t
  | false :: t -> counting_true t
  | [] -> 0;;

let count_nice l = counting_true (List.map nice_sentence l);;

let rec rebuilding lb lines =
  match lb, lines with
  | h1 :: t1, h2 :: t2 when h1 -> h2 :: (rebuilding t1 t2)
  | _ :: t1, _ :: t2 -> rebuilding t1 t2
  | [], _ -> []
  | _, [] -> [];;

(* Question 2 *)
open Hashtbl
(*
let memoizee f = 
  let m = Hashtbl.create 50 in
  let rec g x =
    try
      Hashtbl.find m x
    with Not_found ->
      let y = f g x in
      Hashtbl.add m x y;
      y
  in
  g;;
*)

let double_v2 l =
  let cache = Hashtbl.create 15 in
  let rec aux l i=
    match l with
    | [] -> false
    | [_] -> false
    | (c1 :: c2 :: t) ->
         let x = (c1,c2) in 
             try
               if (Hashtbl.find cache x) != i-1 then true
               else (aux (c2 :: t) (i+1))
             with Not_found ->
               Hashtbl.add cache x i;
               (aux (c2 :: t) (i+1))
  in
  aux l 0;;

let rec muahahah101 =  function
  | [] -> false
  | [_] -> false
  | [_;_] -> false
  | (c1 :: _ :: c3 :: t) when c1 = c3 -> true
  | (c1 :: t) -> muahahah101 t;;

let nice_sentence_V2 sentence =
  let exploded_sentence = exploding_str sentence in
  (double_v2 exploded_sentence) && (muahahah101 exploded_sentence) ;;

let count_nice_V2 l = counting_true (List.map nice_sentence_V2 l);;

(* Printing answer *)

let file = "input.txt"

let lines = read_file_lines file

let () = print_string "\n\nQuest1 answer: "

(*
let nices = List.map nice_sentence lines;;
let () = List.iter (print_endline) (rebuild nices lines);;
*)

let () = print_int (count_nice lines)


(*
let () = print_string "\n\nQuest2 answer: "
let () = print_int (how_many_houses_with_robot lines)
*)

let () = print_string "\n\nQuest2 answer: "

let () = print_int (count_nice_V2 lines)

(*
let nices = List.map nice_sentence_V2 lines;;
let () = List.iter (print_endline) (rebuilding nices lines);;
*)
let () = print_endline "\n\nend of prog"


