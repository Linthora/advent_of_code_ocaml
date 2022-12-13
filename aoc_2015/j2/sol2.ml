open List
open String

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
let get_lw_wh_lh ls =
  match ls with
  | l :: h :: w :: [] -> (l*w , w*h, l*h)
  | _ -> (0,0,0);;

let eval_surface_wrap ls =
  match (get_lw_wh_lh ls) with
  | (lw, wh, lh) -> 2*lw + 2*wh + 2* lh + min lw (min wh lh);;

let tot_surf_wrapping l = List.fold_left (+) 0 (List.map (eval_surface_wrap) (List.map (List.map (int_of_string)) (List.map (split_on_char 'x') l)));;



(* Question 2 *)
let get_2_l_w_h ls =
  match ls with
  | l :: h :: w :: [] -> (l, h, w)
  | _ -> (0,0,0);;

let eval_len_ribbon ls =
  match (get_2_l_w_h ls) with
  | (l, h, w) -> l * h * w + (min (2*(l+w)) (min (2*(l+h)) (2*(w+h))));;

let tot_length_ribbon l = List.fold_left (+) 0 (List.map (eval_len_ribbon) (List.map (List.map (int_of_string)) (List.map (split_on_char 'x') l)));;

(* Printing solutions *)
let file = "input.txt";;
let lines = read_file_lines file;;

let () = print_string "\n\nQuest1 answer: ";;
let () = print_int (tot_surf_wrapping lines);;

let () = print_string "\n\nQuest2 answer: ";;
let () = print_int (tot_length_ribbon lines);;

let () = print_endline "\n\nend of prog";;
