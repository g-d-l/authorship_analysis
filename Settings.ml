open Core.Std

module Settings = 
struct

  (* must be >= 2, obviously *)
  let ngram_size = 3
  
  (* largest expected word length from texts, 30 to be safe *)
  let max_word_length = 30

  (* how many of the top ngrams to compare for Hirschberg *)
  let top_ngram_length = 150
  
  (* total cells in the minimally reduced matrix for Hirschberg,
     should be n^2 for some n >= 2 *)
  let hirsch_mem_lim = 36

  (* types of punctuation to track DON'T CHANGE *)
  type punctuation = {apostrophe : int;
                      comma : int;
                      dash : int; 
                      exclamation : int;
                      period : int;
                      question : int;
                      quotes : int;
                      space : int}

  (* types of conjunctions to track - 
     underscores to escape OCaml keywords like for, if, etc. DON'T CHANGE *)
  type conjunction = {_a : int;
                      _and : int;
                      _as : int;
                      _but : int;
                      _for : int;
                      _if : int;
                      _nor : int;
                      _or : int;
                      _so : int;
                      _the : int; 
                      _yet : int}
end
