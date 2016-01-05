open Author_I
open Core.Std
open Dict
open Settings

class author : author_i =
object

  val mutable name = ""
  val mutable born = 0
  val mutable died = 10000
  val mutable description = ""
  val mutable total_words = 0
  val mutable total_sents = 0
  val mutable length_dist = Array.create ~len:(Settings.max_word_length + 1) 0
  val mutable punct_dist : Settings.punctuation = {apostrophe = 0;
                        comma = 0;
                        dash = 0; 
                        exclamation = 0;
                        period = 0; 
                        question = 0;
                        quotes = 0;
                        space = 0}
  val mutable conj_dist : Settings.conjunction = {_a = 0; 
                           _and = 0;
                           _as = 0;
                           _but = 0;
                           _for = 0;
                           _if = 0;
                           _nor = 0;
                           _or = 0;
                           _so = 0;
                           _the = 0;
                           _yet = 0}
  val mutable dict = Dict.empty

  val mutable top_ngrams = [||]  
  
  method get_name = name
  
  method get_born = born
  
  method get_died = died
  
  method get_dict = dict
  
  method get_description = description
  
  method get_total_words = total_words
  
  method get_total_sents = total_sents
  
  method get_length_dist = length_dist
  
  method get_punct_dist = punct_dist
  
  method get_conj_dist = conj_dist
  
  method get_top_ngrams = top_ngrams
 
  method get_avg_sent_len = 
    (Float.of_int total_words) /. (Float.of_int total_sents)
    
  method update_name name1 = name <- name1
  
  method update_born born1 = born <- born1
  
  method update_died died1 = died <- died1
  
  method update_description description1 = description <- description1
  
  method update_total_words change = total_words <- total_words + change
  
  method update_total_sents change = total_sents <- total_sents + change
  
  method update_length_dist changes = 
    length_dist <- Array.mapi ~f:(fun i x -> x + length_dist.(i)) changes
  
  method update_punct_dist (changes : Settings.punctuation) = punct_dist <- 
    {apostrophe = punct_dist.apostrophe + changes.apostrophe;
     comma = punct_dist.comma + changes.comma;
     dash = punct_dist.dash + changes.dash;
     exclamation = punct_dist.exclamation + changes.exclamation; 
     period = punct_dist.period + changes.period;
     question = punct_dist.question + changes.question;
     quotes = punct_dist.quotes + changes.quotes;
     space = punct_dist.space + changes.space}
  
  method update_conjunc_dist (changes : Settings.conjunction) = 
    conj_dist <- 
    {_a = conj_dist._a + changes._a;
     _as = conj_dist._as + changes._as;
     _and = conj_dist._and + changes._and;
     _but = conj_dist._but + changes._but;
     _for = conj_dist._for + changes._for;
     _if = conj_dist._if + changes._if;
     _nor = conj_dist._nor + changes._nor;
     _or = conj_dist._or + changes._or;
     _so = conj_dist._so + changes._so;
     _the = conj_dist._the + changes._the;
     _yet = conj_dist._yet + changes._yet;}
   
  method update_top_ngrams = top_ngrams <-
  
    (* insert a new value into a list sorted from smallest (head)
       largest (tail *)
    let top_insert (lst : (string * int) list) (e : string * int) = 
      
      let rec insert_rec (lst : (string * int) list) (e : string * int) : 
                         (string * int) list = 
        let (_, i) = e in
        match lst with
        | [] -> [e]
        | (str1, i1) :: [] -> if i >= i1 then (str1, i1) :: [e]
                              else e :: [(str1, i1)]
        | (str1, i1) :: tl -> if i <= i1 then e :: lst
                              else (str1, i1) :: insert_rec tl e in
      
      (* if the list is longer than the desired number of ranked ngrams,
         trim one off (invariant: list no more than 1 greater *)
      let result = insert_rec lst e in
      if List.length result > Settings.top_ngram_length then 
        match result with
        | _ :: tl -> tl
        | _ -> []
      else result in
      
      (* fold this insertion function across the dictionary,
         then unzip to get ngrams only *)
      let ks, _ = List.unzip (Dict.fold (fun k v lst -> top_insert lst (k, v))
      [] dict) in
                                     
      Array.of_list ks
                                     
  method add_ngram ngram = dict <- Dict.insert dict ngram 1;
 
end
