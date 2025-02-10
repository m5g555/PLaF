(*
WORDLE GAME
Author: 
*)
(* Install opam package "csv" with "opam install csv" *)
#require "csv"


type game_state = { w_h : string list;
                    kb : string list;
                    the_word: string }

(* initial state *)
let state =
  { w_h = ["_____";"_____";"_____";"_____";"_____";"_____"];
    kb = ["QWERTYUIOP";" ASDFGHJKL";"  ZXCVBNM"];
    the_word = "OCAML"
  }



let word_bank_src = Csv.load "word_bank.csv"


let rec process_data src =
  match src with
  | [] -> []
  | h::t -> 
    (match h with 
    | [] -> []
    | [u] -> (String.uppercase_ascii u) :: process_data t) 



let process_data_fold src =
	(* TODO *)
  List.fold_right (fun a b -> String.uppercase_ascii a) (List.fold_right (fun a b -> a::b) src []) ""


let word_bank = process_data word_bank_src


let is_word_valid w =
  (* TODO *)
	List.mem w word_bank




let string_to_list s = List.init (String.length s) (String.get s)

let rec space_out_word s =
  
  let rec insert_spaces_in_list l = 
    match l with
    | h::t -> h::" ".[0]::insert_spaces_in_list t
    | [] -> []
  in

  let string_to_spaced_list s = insert_spaces_in_list (string_to_list s) in
  
  String.of_seq (List.to_seq (string_to_spaced_list s))
		

let rec rm_letter kb letter =
	(* TODO *)
  let rec letterchecker s = match string_to_list s with
    | [] -> []
    | h::t -> if h=letter then " ".[0]::t else h::letterchecker (String.of_seq (List.to_seq(t)))
  in
  
  match kb with 
  | [] -> []
  | h::t -> (String.of_seq (List.to_seq(letterchecker h)))::rm_letter t letter

let rec new_kb kb word idx =
	(* TODO *)
  if idx < String.length word then (
    new_kb (rm_letter kb word.[idx]) word (idx+1)
  ) else (kb)


let fill_in_the_blanks blanks word =
	(* TODO *)
  let correct_word_list = string_to_list state.the_word in
  let previous_letters_list = string_to_list blanks in
  let guess_letters_list = string_to_list word in

  List.iter2 (fun a b)correct_word_list guess_letters_list

	raise (Failure "Not Implemented")


let rm_dup strlst = 
	(* TODO *)
	raise (Failure "Not Implemented")


let get_good_letters word =
	(* TODO *)
	raise (Failure "Not Implemented")


let pcl_helper blanks good_letters =
	(* TODO *)
	raise (Failure "Not Implemented")
	

let print_correct_letters blanks good_letters =
  begin
    print_endline "--------------------------------";
    print_endline ("-- letters in right spots: "^(space_out_word blanks));
    print_endline ("-- correct letters       : "^(pcl_helper blanks good_letters));
    print_endline "--------------------------------"
  end


let print_word str =
	print_endline (space_out_word str)


let print_word_history wh =
	begin
		print_word (List.nth wh 0);
		print_word (List.nth wh 1);
		print_word (List.nth wh 2);
		print_word (List.nth wh 3);
		print_word (List.nth wh 4);
		print_word (List.nth wh 5)
	end


let game_over_billboard =
  ["---------------";
   "-- GAME OVER --";
   "---------------"]

let not_valid_word_billboard =
  [ "----------------------------------------";
    "-- Not a valid word!					 --";
    "-- Reenter your guess(5 letter word): "]

let you_won_billboard =
  [ "-----------------";
    "-- YOU WON !!! --";
    "-----------------";
    ""]

let enter_your_guess_billboard =
  [ "";
    "= = = = = = = = = = = = = = =";
    "";
    "Enter your guess (5 letter word): "]

let print_hud state word_entered blnk gl = 
  if word_entered = state.the_word
  then
    begin
      List.iter print_endline you_won_billboard;
      List.iter print_word state.w_h; (* print word history *)
      failwith "END GAME"
    end
  else
    begin
      print_correct_letters blnk gl;
      print_endline "";
      List.iter print_word state.w_h; (* print word history *)
      print_endline "";
      List.iter print_word state.kb; (* print keyboard *)
      List.iter print_endline enter_your_guess_billboard
    end


let rec game_loop state blnk gl num_tries=
  if num_tries = 6
  then 
    begin
      List.iter print_endline game_over_billboard;
      failwith "END GAME"
    end
  else
    let s = read_line ()
    in let s_upper = String.uppercase_ascii s
    in 
    if is_word_valid s_upper
    then
      (let new_blnk = fill_in_the_blanks blnk s_upper
       in let new_gl = gl @ (get_good_letters s_upper)
       in let new_word_history = (List.mapi (fun i word -> if i=num_tries then s_upper else word) state.w_h)
       in let new_key_board = (new_kb state.kb s_upper 0)
       in let new_state = {state with
                           w_h=new_word_history; kb=new_key_board}
       in
       begin
         print_hud new_state s_upper new_blnk new_gl;
	 game_loop new_state new_blnk new_gl (num_tries+1)
       end)
    else
      (match s_upper with
       |"QUIT" | "EXIT" -> failwith "Quit"
       | _ ->
	 begin
	   List.iter print_endline not_valid_word_billboard;
	   game_loop state blnk gl num_tries
         end
      )


let () =
  begin
    print_hud state "_____" "_____" [];
    game_loop state "_____" [] 0
  end
