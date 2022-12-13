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

(* pb 1 *)

let rec which_one maxiCount count lines =
  match lines with
  | [] -> (max maxiCount count)
  | ("" :: t) -> which_one (max maxiCount count) 0 t
  | (x :: t) -> which_one maxiCount (count + (int_of_string x)) t;;

(* pb 2 *)

let adjust3 l v =
  let rec aux liste value index =
    match liste, index with
    | _, 3 -> []
    | [], _ -> [v]
    | (x :: t), _ -> (max value x) :: (aux t (min value x) (index+1)) in
  aux l v 0


let rec sum_top_3 l liste_max current =
  match l with
  | [] -> List.fold_left (+) 0 (adjust3 liste_max current)
  | ("" :: t) -> sum_top_3 t (adjust3 liste_max current) 0
  | (x :: t) -> sum_top_3 t liste_max (current + (int_of_string x))



(* printing sols *)


let file = "input.txt"
let lines = read_file_lines file

let () = print_string "Quest1 answer: "
let () = print_int (which_one 0 0 lines)

let () = print_string "\nQuest2 answer: "
let () = print_int (sum_top_3 lines [0] 0)

let () = print_endline "\n\nend of prog"
