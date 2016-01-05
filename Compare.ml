open Author
open Compare_I
open Core.Std
open Hirschberg
open Settings

module Compare : COMPARE =
struct
  open Settings

  (* find each word length frequency as a percentage of all words for
     each author, then find average sum of differences *)
  let word_len_test auth1 auth2 =
    let arr1 = auth1#get_length_dist in
    let arr2 = auth2#get_length_dist in
    let diff_sum = ref 0. in
    let _ = for i = 1 to Settings.max_word_length do
      let p1 = (Float.of_int arr1.(i)) /. (Float.of_int auth1#get_total_words) in
      let p2 = (Float.of_int arr2.(i)) /. (Float.of_int auth2#get_total_words) in
      diff_sum := !diff_sum +. (Float.abs (p2 -. p1))
    done in
      !diff_sum /. (Float.of_int Settings.max_word_length)  
    
    
  (* find percentage difference between them *)  
  let sent_len_test auth1 auth2 =
    let a1 = auth1#get_avg_sent_len in
    let a2 = auth2#get_avg_sent_len in
      1. -. (min a1 a2) /. (max a1 a2)
  
  
  
  (* same method as for word lengths, but this time for the
     punctuation *)  
  let punct_test auth1 auth2 =
  let punct1 = auth1#get_punct_dist in
  let punct2 = auth2#get_punct_dist in
  let sent1 = auth1#get_total_sents in
  let sent2 = auth2#get_total_sents in
    
  let err1 = (Float.of_int (punct1.comma + punct1.dash + punct1.exclamation + 
             punct1.period + punct1.question + punct1.quotes +
             punct1.space)) /. (Float.of_int sent1) in
   
  let err2 = (Float.of_int (punct2.comma + punct2.dash + punct2.exclamation + 
             punct2.period + punct2.question + punct2.quotes +
             punct2.space)) /. (Float.of_int sent2) in
    
  (Float.abs (err1 -. err2)) /. 7.
               
    
  (* same method as for word lengths, but this time for the
     conjunctions *)  
  let conj_test (auth1 : author) (auth2 : author) =
    let conj1 = auth1#get_conj_dist in
    let conj2 = auth2#get_conj_dist in
    let sent1 = auth1#get_total_sents in
    let sent2 = auth2#get_total_sents in
  
    let err1 = (Float.of_int (conj1._a + conj1._and + conj1._as + 
               conj1._but + conj1._for + conj1._if +
               conj1._nor + conj1._or + conj1._so + conj1._the + 
               conj1._yet)) /. (Float.of_int sent1) in
   
    let err2 = (Float.of_int (conj2._a + conj2._and + conj2._as + 
               conj2._but + conj2._for + conj2._if +
               conj2._nor + conj2._or + conj2._so + conj2._the + 
               conj2._yet)) /. (Float.of_int sent2) in
      
    Float.abs (err1 -. err2)
    
    
  (* take two n gram arrays and produce a 
     cost difference between them *)
  let hirschberg_test auth1 auth2 =
    let raw_score = Hirschberg.final_algorithm auth1#get_top_ngrams 
                      auth2#get_top_ngrams in
    1. -. (Float.of_int raw_score) /. (Float.of_int Settings.top_ngram_length)
    
  
    (* aggregate the above 5 tests' results into a final score *)
  let aggregate auth1 auth2 =
    let s1 = word_len_test auth1 auth2 in
    let s2 = sent_len_test auth1 auth2 in
    let s3 = punct_test auth1 auth2 in
    let s4 = conj_test auth1 auth2 in
    let s5 = hirschberg_test auth1 auth2 in
      s1 +. s2 +. s3 +. s4 +. s5
  
end
