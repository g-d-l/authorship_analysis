open Core.Std
open Settings

(* the main (most complex) algorithm used in comparison. It takes two 
   sequences (arrays) and finds the optimal alignment between them, 
   where optimal means the fewest mismatches or gaps of individual elements.
   Doing this on ngrams finds how close the style and word techinque is between
   two authors.
   
   The complexity comes from its space and time requirements. Though it is  
   Big O of mn is running time, we can achieve Big O of m + n in space
   (where m and n are the lengths of each sequence). This is done by 
   recursively breaking down each strand in subdivisions, and only calculating
   the necessary m' x n' matrix of possible scores once it is below a certain
   threshold, thus achieving a fixed memory usage as a base case. *) 

module Hirschberg = 
struct

(* produce the cost score for a pairing (higher is a better match *)
  let score a b = 
    match a, b with
    | _, None -> (-2)
    | None, _ -> (-2)
    | Some a, Some b -> if a = b then 1 else (-1)


  (* find the maximum of 3 ints *)
  let max_three (a : int) (b : int) (c : int) : int = 
    max a (max b c)
  
  
  (* trim the front and back None elements from a 2-row matrix
     invariant: There is some non-None subarray that can be returned *)           
  let alignment_trimmer (arr : string option array array) = 

    let rec find_ends (arr : string option array array) (a : int) (b : int) = 
      match arr.(0).(a), arr.(1).(a), arr.(0).(b), arr.(1).(b) with
      | None, None, None, None -> find_ends arr (a + 1) (b - 1)
      | _, _, None, None -> find_ends arr (a) (b - 1)
      | None, None, _, _ -> find_ends arr (a + 1) (b)
      | _, _, _, _ -> (a, b) in
  
    let (a, b) = find_ends arr 0 (Array.length arr.(0) - 1) in
    [|Array.slice arr.(0) a (b + 1); Array.slice arr.(1) a (b + 1)|]
  
  
  (* produce a matrix of possible scores in mn space 
     (used for base case later on) *)
  let array_matrix (a : string array) (b : string array) : int array array = 
    let x = Array.length a in
    let y = Array.length b in
    let m = Array.make_matrix ~dimx:(x + 1) ~dimy:(y + 1) 0 in

    let _  = Array.set m.(0) 0 0 in
  
    (* go down 0th column *)
    let _ = for i = 1 to x do
      Array.set m.(i) 0 (i * (score (Some a.(i - 1)) None))
    done in
  
    (* go across 0th row *)
    let _ = for j = 1 to y do
      Array.set m.(0) j (j * (score None (Some b.(j - 1))))
    done in
  
    (* fill in rest of matrix *)
    let _ = for i = 1 to x do
      (for j = 1 to y do
        Array.set m.(i) (j) (max_three
                            (m.(i - 1).(j) + (score (Some a.(i - 1)) None))
                            (m.(i - 1).(j - 1) + (score (Some a.(i - 1)) (Some b.(j - 1))))
                            (m.(i).(j - 1) + (score None (Some b.(j - 1)))))
      done)
    done in
       m
    
    
  (* return the optimal alignment based on the previous 
     generated matrix of scores *)
  let matrix_to_opt (a : string array) (b : string array) 
                     (m : int array array) =
    let x = Array.length a in
    let y = Array.length b in
    let output = Array.make_matrix ~dimx:2 ~dimy:(x + y + 1) None in
    let index = ref 0 in

    let rec matrix_to_opt_rec (i : int) (j : int) =
    if i = 0 && j = 0 then
      let _ = Array.rev_inplace output.(0) in
      Array.rev_inplace output.(1)
    else if i > 0 && m.(i).(j) = m.(i - 1).(j) + score (Some a.(i - 1)) None 
      then
      let _ = matrix_to_opt_rec (i - 1) j in
      let _ = index := !index + 1 in
      let _ = Array.set output.(0) (!index) (Some a.(i - 1)) in
      Array.set output.(1) (!index) None
    else if i > 0 && j > 0 && m.(i).(j) = m.(i - 1).(j - 1) + 
            score (Some a.(i - 1)) (Some b.(j - 1)) then
      let _ = matrix_to_opt_rec (i - 1) (j - 1) in
      let _ = index := !index + 1 in
      let _ = Array.set output.(0) (!index) (Some a.(i - 1)) in
      Array.set output.(1) (!index) (Some b.(j - 1))
    else
      let _ = matrix_to_opt_rec (i) (j - 1) in
      let _ = index := !index + 1 in
      let _ = Array.set output.(0) (!index) None in
      Array.set output.(1) (!index) (Some b.(j - 1))
    in
       
    let _ = matrix_to_opt_rec x y in
    alignment_trimmer output
    
    
  (* like array_matrix, but done in m + n space, used recursively below *)
  let optimal_score (a : string array) (b : string array) =
    let x = Array.length a in
    let y = Array.length b in
    let output = Array.create ~len:(y + 1) 0 in
  
    let _ = for i = 1 to y do
      Array.set output i (i * (score None (Some b.(i - 1))))
    done in 
  
    let _ = for i = 1 to x do
      let copy1 = output.(0) in
      let copy = ref copy1 in
      Array.set output 0 (i * (score (Some a.(i - 1)) None));
      (for j = 1 to y do
        let temp = output.(j) in
        Array.set output j (max_three
                           (output.(j) + (score (Some a.(i - 1)) None))
                           (!copy + (score (Some a.(i - 1)) (Some b.(j - 1))))
                           (output.(j - 1) + (score None (Some b.(j - 1)))));
        copy := temp
        done);
      done in
    output
  

  (* apply previous functions recursively on smaller and smaller
     pieces of the first sequence againat pieces of the second 
     until within hirsch_mem_lim space (to achieve Big O of (x + y) space) *)
  let splitter (a : string array) (b : string array) = 
  
    let x = Array.length a in
    let y = Array.length b in
  
    (* a1, a2, b1, b2 are the indices of sequences a and b *)
    let rec splitter_rec (a1 : int) (a2 : int) (b1 : int) (b2 : int) =
      (* check if small enough sequences *)
      if (a2 - a1 + 1) * (b2 - b1 + 1) <= Settings.hirsch_mem_lim then
        let a_done = (Array.slice a a1 (a2 + 1)) in
        let b_done = (Array.slice b b1 (b2 + 1)) in
        matrix_to_opt a_done b_done (array_matrix a_done b_done)
      (* otherwise, take  the midpoint of a and the optimal point of b,
         where the optimal point is done by finding the maximum score for
         each score array (from optimal_score *)
      else
        let a_mid = (a1 + a2) / 2 in
        let front_scores = optimal_score (Array.slice a a1 (a_mid + 1)) 
                                         (Array.slice b b1 (b2 + 1)) in
        let back_scores = optimal_score (Array.slice a (a_mid + 1) (a2 + 1)) 
                                        (Array.slice b b1 (b2 + 1)) in
        let max_pos = ref 0 in
        let max_score = ref (front_scores.(0) + back_scores.(0)) in
        let _ = for i = 1 to b2 - b1 + 1 do
          let curr_score = front_scores.(i) + back_scores.(i) in
          if curr_score > !max_score then
            (max_score := curr_score;
            max_pos := i)
          done in
        (* recurse on the two subdivision *)
        let front = splitter_rec a1 a_mid b1 (!max_pos + b1 - 1) in
        let back = splitter_rec (a_mid + 1) a2 (!max_pos + b1) b2 in
        
        (* paste the results back together *)
        [|(Array.append front.(0) back.(0)); (Array.append front.(1) back.(1))|]
    in
      
      splitter_rec 0 (x - 1) 0 (y - 1)
    
      
  (* find the total cost between two sequences *) 
  let total_cost (arr : string option array array) = 
    let cost = ref 0 in
    let _ = for i = 0 to (Array.length arr.(0)) - 1 do
      cost := !cost + (score arr.(0).(i) arr.(1).(i))
    done in
      !cost
  
    
  (* put it all together into one function *)
  let final_algorithm (a : string array) (b : string array) =
    total_cost (splitter a b)

end
