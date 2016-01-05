open Author
open Core.Std
open Settings

(* lower scores always indicate a better match between authors *)
module type COMPARE =
sig

  (* compare the average word lengths *)
  val word_len_test : author -> author -> float
    
  (* compare the average sentence lengths *)
  val sent_len_test : author -> author -> float
  
  (* compare the punctuation distributions *)
  val punct_test : author -> author -> float

  (* compare the conjunction distributions *)
  val conj_test : author -> author -> float
  
  (* take two n gram arrays and produce a 
     cost difference between them *)
  val hirschberg_test : author -> author -> float
  
  (* aggregate the above 5 tests' results *)
  val aggregate : author -> author -> float
  
end
