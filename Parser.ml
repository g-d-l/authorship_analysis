open Core.Std
open Settings

(* 2-pass method allows for relatively simple ngram scanning independent of ngram_size.
   For example, if ngram_size = 2, then to read for 3-letter conjunction we would need 
   some way to keep track of previous and next characters as well. But, if ngram_size = 10
   then we have to search the substrings as well. 2-pass keeps things simple *)

module Parser = 
struct

open Settings

  (* filter out new lines and underscores *) 
  let rec get_char input = 

    let c = Char.lowercase (input_char input) in 
    match c with
    | '\n' -> ' '
    | '\r' -> ' '
    | '_' -> get_char input
    | _ -> c


  (* used to turn buffer of ngram into string *)
  let char_list_to_string (lst : char list) = 
    String.concat (List.map ~f:Char.escaped lst)


  (* sum of int array *)
  let array_sum (arr : int array) =

    let rec array_sum_rec (arr : int array) (i : int) = 
      match i with
      | 0 -> arr.(0)
      | _ -> arr.(i) + (array_sum_rec) arr (i - 1) in
  
    array_sum_rec arr (Array.length arr - 1)


  (* parse for ngrams *)
  let parse_ngrams author txt_loc = 
  
    let input = open_in txt_loc in 
    
    (* make inital buffer *)
    let rec make_buffer input count = 
    match count with
    | 0 -> []
    | _ -> (get_char input) :: (make_buffer input (count - 1)) in
  
    let buffer = List.rev (make_buffer input Settings.ngram_size) in
    
    (* add ngram, then swap in the next character *)
    let rec parse_entire input buffer = 
      try
        let c = get_char input in
        let _ = author#add_ngram (char_list_to_string buffer) in
        match buffer with
        | _ :: tl -> parse_entire input (tl @ [c])
        | _ -> ()
      with _ ->
      close_in_noerr input in
   
    parse_entire input buffer
        

  (* scan for punctuation, word lengths, etc. *)
  let parse_other_data author txt_loc =

    let input = open_in txt_loc in
    let sent_count = ref 0 in
    let word_lengths = Array.create ~len:(Settings.max_word_length + 1) 0 in
    let punct_dist = ref {apostrophe = 0;
                          comma = 0;
                          dash = 0; 
                          exclamation = 0;
                          period = 0; 
                          question = 0;
                          quotes = 0;
                          space = 0} in
    let conj_dist = ref {_a = 0;
                         _and = 0;
                         _as = 0;
                         _but = 0; 
                         _for = 0;
                         _if = 0;
                         _nor = 0; 
                         _or = 0;
                         _so = 0;
                         _the = 0;
                         _yet = 0} in
  
  
    let rec parse_entire input buffer = 
      (try
        (let c = get_char input in
        let _ = match c with
                | ',' -> punct_dist := 
                         {!punct_dist with comma = (!punct_dist).comma + 1}
                         (* also record another sentence *)
                | '.' -> let _ = punct_dist := {!punct_dist with 
                         period = (!punct_dist).period + 1} in
                         sent_count := !sent_count + 1
                | ' ' -> punct_dist := 
                         {!punct_dist with space = (!punct_dist).space + 1}
                | '\'' -> punct_dist := {!punct_dist with 
                          apostrophe = (!punct_dist).apostrophe + 1}
                | '-' -> punct_dist := {!punct_dist with 
                         dash = (!punct_dist).dash + 1}
                | '!' -> punct_dist := {!punct_dist with 
                         exclamation = (!punct_dist).exclamation + 1}
                | '?' -> punct_dist := {!punct_dist with 
                         question = (!punct_dist).question + 1}
                | _ -> () in
      let ascii = Char.to_int c in
      (* if it's a letter, continue building word *)
      if (ascii >= 97 && ascii <= 122) || (ascii >= 65 && ascii <= 90) then
        parse_entire input (c :: buffer)
      (* otherwise it's a new word, so... *)
      else
        let len = List.length buffer in
        let _ = if len > 0 then
          (* record length *)
          let _ = Array.set word_lengths len (word_lengths.(len) + 1) in
          let word = char_list_to_string (List.rev buffer) in
          (* filter common uses of period to avoid overcounting sentences *)
          if word = "Mr" || word = "Ms" || word = "Mrs" then
            sent_count := !sent_count - 1
          else
            (* record conjunctions *)
            match word with
            | "a" -> conj_dist := 
                     {!conj_dist with _a = (!conj_dist)._a + 1}
            | "and" -> conj_dist := 
                       {!conj_dist with _and = (!conj_dist)._and + 1}
            | "as" -> conj_dist := 
                      {!conj_dist with _as = (!conj_dist)._as + 1}
            | "but" -> conj_dist := 
                       {!conj_dist with _but = (!conj_dist)._but + 1}
            | "for" -> conj_dist := 
                       {!conj_dist with _for = (!conj_dist)._for + 1}
            | "if" -> conj_dist := 
                      {!conj_dist with _if = (!conj_dist)._if + 1}
            | "nor" -> conj_dist := 
                       {!conj_dist with _nor = (!conj_dist)._nor + 1}
            | "or" -> conj_dist := 
                      {!conj_dist with _or = (!conj_dist)._or + 1}
            | "so" -> conj_dist := 
                      {!conj_dist with _so = (!conj_dist)._so + 1}
            | "the" -> conj_dist := 
                       {!conj_dist with _the = (!conj_dist)._the + 1}
            | "yet" -> conj_dist := 
                       {!conj_dist with _yet = (!conj_dist)._yet + 1}
            | _ -> ()
       
        in
          parse_entire input [])
          

    with _ ->
      close_in_noerr input) in
  
    (* apply updates *)
    let _ = parse_entire input [] in
    let _ = author#update_total_words (array_sum word_lengths) in
    let _ = author#update_total_sents !sent_count in  
    let _ = author#update_length_dist word_lengths in  
    let _ = author#update_punct_dist !punct_dist in
    author#update_conjunc_dist !conj_dist


(* put above 2 primary functions together *)
let complete_parse author txt_loc = 
  let _ = parse_ngrams author txt_loc in
  parse_other_data author txt_loc

end
