open Core.Std
open Dict
open Settings

(* all self-explanatory *)

class type author_i =
object

  method get_name : string
  
  method get_born : int
  
  method get_description : string
  
  method get_died : int
  
  method get_dict : Dict.dict
  
  method get_total_words : int
  
  method get_total_sents : int
  
  method get_length_dist : int array
  
  method get_punct_dist : Settings.punctuation
  
  method get_conj_dist : Settings.conjunction
  
  method get_top_ngrams : string array
  
  method get_avg_sent_len : float
  
  method update_name : string -> unit
  
  method update_born : int -> unit
  
  method update_died : int -> unit
  
  method update_description : string -> unit
 
  method update_total_words : int -> unit
  
  method update_total_sents : int -> unit
  
  method update_length_dist : int array -> unit
  
  method update_punct_dist : Settings.punctuation -> unit
  
  method update_conjunc_dist : Settings.conjunction -> unit
   
  method update_top_ngrams : unit
                                     
  method add_ngram : string -> unit
  
end
