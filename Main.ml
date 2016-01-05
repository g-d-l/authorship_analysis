open Author
open Compare
open Core.Std
open Dict
open Parser


type index_entry = {name : string; born : int; died : int; 
                    description : string; texts : string list}


(* scan index to build a list of texts to parse *)
let build_index () = 
  let input = open_in "texts/index.txt" in
  
  let rec index_parse entry lst = 
    try
      let pre_line = input_line input in
      let line = String.slice pre_line 0 (String.length pre_line - 1) in
      match String.get line 0 with
      | '1' -> index_parse {entry with born = (Int.of_string line)} lst
      | '#' -> index_parse {entry with died = 
                            (Int.of_string (String.slice line 1 5))} lst
      | '*' -> index_parse {entry with description = line} lst
      | 't' -> index_parse {entry with texts = line :: entry.texts} lst
      | '~' -> index_parse {name = ""; born = 0; died = 0; description = ""; 
                            texts = []} (entry :: lst)
      | _ -> index_parse {entry with name = line} lst
      
    with _ ->
      let _ = close_in_noerr in
    lst in
  
  index_parse {name = ""; born = 0; died = 0; description = ""; texts = []} []
  
  
(* make an author object for each author, add info,
   and parse his/her texts *)
let rec build_database index = 
  match index with
  | [] -> []
  | hd :: tl -> let new_author = new author in
                let _ = new_author#update_name hd.name in
                let _ = new_author#update_born hd.born in
                let _ = new_author#update_died hd.died in
                let _ = new_author#update_description hd.description in
                let _ = for i = 0 to List.length hd.texts - 1 do
                  match List.nth hd.texts i with
                  | Some txt -> Parser.complete_parse new_author txt
                  | _ -> ()
                  done in
                let _ = new_author#update_top_ngrams in
                new_author :: (build_database tl)
                
                
(* scan command line arguments to extract mystery text location
   also handle possible start and/or end year range *)           
let parse_args () : (string * (int option) * (int option)) =
  let usage () = Printf.printf "\nUsage: %s " Sys.argv.(0);
    print_string "text_location from:start to:end\n\nMake sure file location ";
    print_string "is valid.\n\nStart and end years (integers) are each ";
    print_string "optional,\nbut from: and to: tags must both be used, even";
    print_string " if left empty\n(they must come in that order, too).\n\n" in
    
    if Array.length Sys.argv <> 4 then 
      let _ = usage () in exit 1 
    else
      (* make sure file exists and opens and years are actually years *)
      let from_len = String.length Sys.argv.(2) in
      let to_len = String.length Sys.argv.(3) in
      match from_len, to_len with
      | 5, 3 -> (try let _ = open_in Sys.argv.(1) in 
                      (Sys.argv.(1), None, None)
                    with _ -> let _ = usage () in exit 1 )
      | _, 3 -> (try let _ = open_in Sys.argv.(1) in
                    let born = String.slice Sys.argv.(2) 5 from_len in
                    let year = int_of_string born in
                      (Sys.argv.(1), Some year, None)
                 with _ ->
                  let _ = usage () in exit 1)
      | 5, _ -> (try let _ = open_in Sys.argv.(1) in
                    let died = String.slice Sys.argv.(3) 3 to_len in
                    let year = int_of_string died in
                      (Sys.argv.(1), None, Some year)
                 with _ ->
                  let _ = usage () in exit 1)
      | _, _ -> (try let _ = open_in Sys.argv.(1) in
                    let born = String.slice Sys.argv.(2) 5 from_len in
                    let died = String.slice Sys.argv.(3) 3 to_len in
                    let year1 = int_of_string born in
                    let year2 = int_of_string died in
                      (Sys.argv.(1), Some year1, Some year2)
                 with _ ->
                  let _ = usage () in exit 1)


(* make rankings by comparing each author to the new mystery one,
   then sorting list *)
let rec make_rankings mystery_entry database = 
  match database with
  | [] -> []
  | hd :: tl -> (hd, Compare.aggregate hd mystery_entry) :: 
                 make_rankings mystery_entry tl


(* print the final ranked array of authors and scores 
   along with the blurb of the top author *)
let print_array arr =
  let _ = print_string "\n" in
  let arr_length = Array.length arr in
  match arr_length with
  | 0 -> 
      print_string "\nNo matches found! Are you sure the potential author ";
      print_string "is in the database?\n"
  | _ ->
    let _ = for i = 0 to arr_length - 1 do
      let (author, score) = arr.(i) in
      Printf.printf "%d) %.3f -- %s\n" (i + 1) score author#get_name
    done in
    let (author, _) = arr.(0) in
    print_string "\n"; print_string author#get_description; print_string "\n\n"


(* combine above functions and run *)
let (new_loc, y1, y2) = parse_args () in
let index = build_index () in
let database = build_database index in
let mystery_entry = new author in
let _ = match y1, y2 with
        | Some born, Some died -> let _ = mystery_entry#update_born born in
                                    mystery_entry#update_died died
        | Some born, None -> mystery_entry#update_born born
        | None, Some died -> mystery_entry#update_died died
        | _, _ -> () in

let _ = Parser.complete_parse mystery_entry new_loc in

(* filter out based on given birth/death years, if provided *)
let new_database = match mystery_entry#get_born, mystery_entry#get_died with
                   | 0, 0 -> database
                   | 0, _ ->
                       List.filter
                       ~f:(fun a -> a#get_born <= mystery_entry#get_died)
                       database
                   | _, 0 ->
                       List.filter
                       ~f:(fun a -> a#get_died >= mystery_entry#get_born) 
                       database
                   | _, _ -> 
                       List.filter
                       ~f:(fun a -> not (a#get_born > mystery_entry#get_died
                       || a#get_died < mystery_entry#get_born)) database in

(* sort rankings list *)
let rankings = (List.sort 
               ~cmp:(fun (_, s1) (_, s2) -> 
                     if s1 > s2 then 1
                     else if s1 = s2 then 0
                     else (-1))
               (make_rankings mystery_entry new_database)) in 



(* display results! *)                     
print_array (Array.of_list rankings);;            
