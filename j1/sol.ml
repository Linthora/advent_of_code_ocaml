open String
open List

let () = print_endline "Hello World\nEntering file:"

let read_file_lines filename=
  let lines = ref [] in
  let fp = open_in filename in
    try
      while true; do
        lines := input_line fp :: !lines
      done; !lines
      (*
      let l = input_line ic in
        (*print_endline l;*)
        flush stdout;
        close_in ic;
        !l*)
    with End_of_file ->
      close_in fp;
      List.rev !lines;;

let exploding_str s = List.init (String.length s) (String.get s);;

let rec counting lc =
  match lc  with
  | '(' :: t -> 1 + counting t
  | ')' :: t -> -1 + counting t
  | _ -> 0;;

let counter s = counting (exploding_str s)

let rec entering_basement_index cur_height lc =
  match cur_height, lc with
  | -1, _ -> 0
  | _, '(' :: t -> 1 + entering_basement_index (cur_height + 1) t
  | _, ')' :: t -> 1 + entering_basement_index (cur_height -1) t
  | _, _ -> 0;;

let basement_entrance s = entering_basement_index 0 (exploding_str s);;


let file = "input.txt"
let lines = read_file_lines file

let () = print_string "Quest1 answer: "
let () = print_int (counter (hd lines))

let () = print_string "\nQuest2 answer: "
let () = print_int (basement_entrance (hd lines))

let () = print_endline "\nfile Done\n"
