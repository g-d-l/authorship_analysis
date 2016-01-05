open Core.Std

(* Adapted from the 2-3 Balanced Tree used in the Moogle PSet -
   only changed the types make it work for ngram strings and frequency ints
   rather than keywords and URLs *)
   
module Dict =
struct

  type key = string
  type value = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_key (k : key) = k
  let string_of_value = string_of_int


  type pair = key * value

  type dict =
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS:
   * 2-node: Two(left,(k1,v1),right)
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1.
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.
   *
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right)
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1.
   * (3) Every key k appearing in subtree right must be k > k2.
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three
   *     subtrees must be the same.
   *)

  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  let empty : dict = Leaf

  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two (l, p, r) -> 
        let (k, v) = p in
        (fold f (f k v (fold f u l)) r)
    | Three (l, p1, m, p2, r) -> 
        let (k1, v1) = p1 in
        let (k2, v2) = p2 in
        (fold f (f k2 v2 (fold f (f k1 v1 (fold f u l)) m)) r)

  let string_of_key = string_of_key
  let string_of_value = string_of_value

  let rec string_of_dict (d: dict) : string = 
    match d with
    | Leaf -> ""
    | Two (l, (k, v), _) -> 
        let p_str = (string_of_key k) ^ "->" ^ (string_of_value v) ^ ", " in
          (match l with
          | Leaf -> p_str
          | _ -> (string_of_dict l) ^ p_str ^ (string_of_value v))
    | Three (l, (k1, v1), m, (k2, v2), r) -> 
        let p1_str = (string_of_key k1) ^ "->" ^ (string_of_value v1) ^ ", " in
          let p2_str = (string_of_key k2) ^ "->" ^ (string_of_value v2) ^ ", " in
            (match l with
            | Leaf -> p1_str ^ p2_str
            | _ -> (string_of_dict l) ^ p1_str ^ (string_of_dict m) ^ p2_str ^ 
                   (string_of_dict r))

  let rec string_of_tree (d: dict) : string =
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ ","
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"


  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up (Leaf, (k, v), Leaf) 
      | Two(left, n, right) ->  insert_downward_two (k, v) n left right 
      | Three(left, n1, middle, n2, right) -> 
          insert_downward_three (k, v) n1 n2 left middle right 

 
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
    match compare k k1 with
    | Equal -> Done (Two (left, (k, v + v1), right))
    | Less -> 
        (match insert_downward left k v with
        | Up (l, (k2, v2), r) -> Done (Three (l, (k2, v2), r, (k1, v1), right))
        | Done x -> Done (Two (x, (k1, v1), right)))
    | Greater -> 
        (match insert_downward right k v with
        | Up (l, (k2, v2), r) -> Done (Three (left, (k1, v1), l, (k2, v2), r))
        | Done x -> Done (Two (left, (k1, v1), x)))

  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
        match compare k k1, compare k k2 with
        | Equal, _ -> Done (Three (left, (k, v + v1), middle, (k2, v2), right))
        | Less, Less -> 
            (match insert_downward left k v with
            | Up (l, (k3, v3), r) -> Up ((Two (l, (k3, v3), r)), (k1, v1),
                                     (Two (middle, (k2, v2), right)))
            | Done x -> (Done (Three (x, (k1, v1), middle, (k2, v2), right))))
        | Greater, Less -> 
            (match insert_downward middle k v with
            | Up (l, (k3, v3), r) -> Up ((Two (left, (k1, v1), l)), (k3, v3),
                                     (Two (r, (k2, v2), right)))
            | Done x -> (Done (Three (left, (k1, v1), x, (k2, v2), right))))
        | _, Equal -> Done (Three (left, (k1, v1), middle, (k, v + v2), right))
        | Greater, Greater -> 
            (match insert_downward right k v with
            | Up (l, (k3, v3), r) -> Up ((Two (left, (k1, v1), middle)), 
                                        (k2, v2), (Two (l, (k3, v3), r)))
            | Done x -> Done (Three (left, (k1, v1), middle, (k2, v2), x)))
        | _ -> raise (Failure "Invariant error found in insert_downward_three")
            

  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

 
  let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2, x, l, Two(m, y, r) -> Hole (rem, Three (l, x, m, y, r))
      | Right2, y, Two (l, x, m), r -> Hole (rem, Three (l, x, m, y, r))
      | Left2, x, a, Three(b, y, c, z, d) -> 
          Absorbed (rem, Two (Two (a, x, b), y, Two(c, z, d)))
      | Right2, z, Three (a, x, b, y, c), d -> 
          Absorbed (rem, Two (Two (a, x, b), y, Two (c, z, d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed (rem, Two (Leaf, n, Leaf))

 
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3, x, z, a, Two (b, y, c), d -> 
          Absorbed (rem, Two (Three (a, x, b, y, c), z, d))
      | Mid3, y, z, Two (a, x, b), c, d -> 
          Absorbed (rem, Two (Three (a, x, b, y, c), z, d))
      | Mid3, x, y, a, b, Two (c, z, d) -> 
          Absorbed (rem, Two (a, x, Three (b, y, c, z, d)))
      | Right3, x, z, a, Two (b, y, c), d -> 
          Absorbed (rem, Two (a, x, Three (b, y, c, z, d)))
      | Left3, w, z, a, Three (b, x, c, y, d), e -> 
          Absorbed (rem, Three (Two (a, w, b), x, Two (c, y, d), z, e))
      | Mid3, y, z, Three (a, w, b, x, c), d, e -> 
          Absorbed (rem, Three (Two (a, w, b), x, Two (c, y, d), z, e))
      | Mid3, w, x, a, b, Three (c, y, d, z, e) -> 
          Absorbed (rem, Three (a, w, Two(b, x, c), y, Two(d, z, e)))
      | Right3, w, z, a, Three(b, x, c, y, d), e -> 
          Absorbed (rem, Three (a, w, Two (b, x, c), y, Two (d, z, e)))
      | Left3, _, _, _, _, _ | Mid3, _, _, _, _, _ | Right3, _, _, _, _, _ ->
          Absorbed (rem, Three (Leaf, n1, Leaf, n2, Leaf))

  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match compare k k1 with
          | Equal -> Hole(Some(k1,v1 - 1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match compare k k1, compare k k2 with
          | Equal, _ -> Absorbed(Some(k1,v1 - 1),Two(Leaf,(k2,v2),Leaf))
          | _, Equal -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1 - 1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =
    match compare k k1 with
      | Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match compare k k1, compare k k2 with
      | Equal, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) ->
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'


  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two (l, (k1, v1), r) -> 
        (match compare k k1 with
        | Less -> lookup l k
        | Equal -> Some v1
        | _ -> lookup r k )
    | Three (l, (k1, v1), m, (k2, v2), r) ->
        (match compare k k1, compare k k2 with
        | Less, Less -> lookup l k
        | Equal, Less -> Some v1
        | Greater, Less -> lookup m k
        | Greater, Equal -> Some v2
        | Greater, Greater -> lookup r k
        | _ -> None)

  
  let member (d: dict) (k: key) : bool =
    match (lookup d k) with
    | None -> false
    | _ -> true

  
  let choose (d: dict) : (key * value * dict) option =
    match d with 
    | Leaf -> None
    | Two (_, (k1, v1), _) -> let new_d = remove d k1 in
                                       Some (k1, v1, new_d)
    | Three (_, (k1, v1), _, _, _) -> let new_d = remove d k1 in
                                               Some (k1, v1, new_d)

  let rec balanced_helper (d: dict) : bool * int =
    match d with
    | Leaf -> (true, 0)
    | Two (l, _, r) -> 
        let (bl, hl) = balanced_helper l in
        let (br, hr) = balanced_helper r in
        let b = bl && br && (hl = hr) in
        let h = 1 + (max hl hr) in
          (b, h)
    | Three (l, _, m, _, r) ->
        let (bl, hl) = balanced_helper l in
        let (bm, hm) = balanced_helper m in
        let (_, hr) = balanced_helper r in
        let b = bl && bm && bl && (hl = hm) && (hl = hr) in
        let h = 1 + (max hm (max hr hl)) in
          (b, h)

  let balanced (d: dict) : bool =
    let (b, _) = balanced_helper d in b
    
end
