open String
open Digest
open Str

let () = print_endline "Hello World\nDay4"

(* Question 1 *)
let eval key i = (Digest.to_hex (Digest.string (key ^ Int.to_string i)));;

let rec searching key r i =
  match (Str.string_match r (eval key i) 0) with
  | true -> i
  | _ -> searching key r (i+1);;




(* Printing answer *)

let puzzle_input = "yzbqklnj"

let reg1 = Str.regexp "^00000.*"
let () = print_string "\n\nQuest1 answer: "
let () = print_int (searching puzzle_input reg1 0)


let reg2 = Str.regexp "^000000.*"
let () = print_string "\n\nQuest2 answer: "
let () = print_int (searching puzzle_input reg2 0)

(*
let () = print_string "\n\nQuest2 answer: "
let () = print_int (how_many_houses_with_robot lines)
*)

let () = print_endline "\n\nend of prog"
