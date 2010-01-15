  exception InpNotDefined ;;
  type boolean = T | F ;;
  type variable = Name of string | NameVal of string*boolean ;;
  type 'a optional  = Some of 'a | None ;;

  (*TODO: how to get this into the List namespace?*)
  let uniqify lst = 
    let rec uniq l1 l2 = match l1 with 
      [] -> l2 
    | x::xs -> if (List.mem x l2) then uniq xs l2
               else uniq xs (x::l2)        in
  uniq lst [] ;;


  let log2 x = ceil (log x /. log 2.0) ;;

  let to_bool v = match v with 
      T -> true
    | F -> false ;;

  let b_to_s v = match v with
      T  -> "T"
    | F  -> "F" ;;

  let rec print_bool_lst lst = match lst with 
      [] -> (Printf.printf "\n"); []
    | x::xs -> (Printf.printf "%s " (b_to_s x)); print_bool_lst xs ;;

  type  bexp = Const of boolean 
    |  Inp of string  
    |  And of bexp * bexp
    |  Or  of bexp * bexp
    |  Not of bexp
    |  Xor of bexp * bexp
    ;;

  let and_ x y = match x,y with
      (T,T) -> T
    | _     -> F;;

  let or_ x y = match x,y with
      (_,T) | (T,_) -> T
    | _             -> F ;;

  let n x = match x with
      T -> F
    | F -> T ;;

  let xor x y = match x,y with
      (T, F) | (F, T) -> T
    | _ -> F;;

  let ( *: ) x y =  and_ x y ;;
  let ( +: ) x y =  or_ x y ;;
  let ( ^: ) x y =  xor x y ;;
  let ( !: ) x   =  n x ;; 

  let rec expr_to_str exp = match exp with
    | Const x     ->  (b_to_s x ) 
    | And(x,y)    ->  " (" ^ (expr_to_str x) ^ " * " ^ 
                             (expr_to_str y) ^ ") "
    | Or(x,y)     ->  " (" ^ (expr_to_str x) ^ " + " ^ 
                             (expr_to_str y) ^ ") "
    | Xor(x,y)    ->  " (" ^ (expr_to_str x) ^ " ^ " ^ 
                             (expr_to_str y) ^ ") "
    | Not(x)      ->  " !"^  (expr_to_str x) ^ " " 
    | Inp(x)      ->  " I" ^ x ^ " "

  let rec reduce exp = 
     match exp with
      Const x                -> exp
    | Inp x                  -> exp 
    | Not(Const F)           -> Const T
    | Not(Const T)           -> Const F
    | Not(Inp x)             -> exp 
    | Not(Not(Inp x))        -> (Inp x) 
    | Not(x)                 -> (reduce (Not(reduce x)))
    | And(x,y) when x = y    -> (reduce x)
    | And(Const T, Const T)  -> Const T
    | And(Const F, _) | And(_, Const F)               -> Const F
    | And(Inp x, Inp y)      -> exp
    | And( Not(Inp x),  y)   -> And(Not(Inp x), reduce y)
    | And( x, Not(Inp y))    -> And(reduce x, Not(Inp y))
    | And(Inp x, y)          -> And(Inp x, reduce y)
    | And( x, Inp y)         -> And(reduce x, Inp y)
    | And(x,y)               -> (*Printf.printf "reduce: And(x,y): \n";*) (reduce(And(reduce x,reduce y)))
    | Or(x,y)  when x=y      -> (reduce x)
    | Or(Const T ,_) | Or(_,Const T)                  -> Const T
    | Or(Const F, Const F)                            -> Const F
    | Or(Inp x, Inp y)       -> exp
    | Or( Not(Inp x),  y)    -> Or(Not(Inp x), reduce y)
    | Or( x, Not(Inp y))     -> Or(reduce x, Not(Inp y))
    | Or(Inp x, y)           -> Or(Inp x, reduce y)
    | Or( x, Inp y)          -> Or(reduce x, Inp y)
    | Or(x,y)                -> (reduce (Or(reduce x,reduce y)))
    | Xor(Const T,Const T) | Xor(Const F ,Const F)    -> Const F
    | Xor(Const T,Const F) | Xor(Const F, Const T)    -> Const T 
    | Xor(Inp x, Inp y)      -> exp
    | Xor( Not(Inp x),  y)   -> Xor(Not(Inp x), reduce y)
    | Xor( x, Not(Inp y))    -> Xor(reduce x, Not(Inp y))
    | Xor(Inp x, y)          -> Xor(Inp x, reduce y)
    | Xor( x, Inp y)         -> Xor(reduce x, Inp y)
    | Xor(x,y)  -> (reduce (Xor(reduce x, reduce y)))

      ;;

  let rec demorganize exp = match exp with 
    | And(Not x, Not y)   -> Not( Or( x, y)) 
    | Or( Not x, Not y)   -> Not( And( x, y)) 
    | Not( Or(x,y) )      -> And(Not x, Not y)
    | Not( And(x,y) )     -> Or(Not x, Not y)
    | _ -> exp ;;

  let rec eval exp env = match exp with
      Const x     -> x
    | And( x,y)   -> ( eval x env) *: ( eval y env) 
    | Or(x,y)     -> ( eval x env) +: ( eval y env)
    | Not(x)      -> n (eval x env) 
    | Xor(x,y)    -> xor (eval x env) (eval y env) 
    | Inp(x)      -> env(exp) (*(Hashtbl.find env x)*)   ;;

  let rec get_inputs exp = match exp with 
      Const x     -> []
    | Inp x       -> [exp]
    | And(x,y) | Or(x,y) | Xor(x,y) -> (get_inputs x) @ (get_inputs y)
    | Not x       -> get_inputs x ;;
 

  let rec literal_count exp = 
    let rec count_literals exp' lc = match exp' with
    | Inp(x)   -> 1
    | Const(x) -> 0
    | And(x,y) | Or(x,y) | Xor(x,y) -> (lc + ( count_literals x lc) + ( count_literals y lc) ) 
    | Not(x)   -> (lc + ( count_literals x lc)) in 
      count_literals exp 0  ;;

  let rec op_count exp = 
    let rec count_ops exp' oc = match exp' with
    | Inp(_) | Const(_) ->  0
    | Not(x) ->  0 (*( count_ops x oc) not counting Not as op*)
    | And(x,y) | Or(x,y) | Xor(x,y) -> (1 + (count_ops x oc) + (count_ops y oc)) in
      count_ops exp 0 
    ;;


let mk_and a b = And(a,b) ;;
let mk_or  a b = Or(a,b)  ;;
let mk_xor a b = Xor(a,b) ;;
let mk_not a   = Not(a)   ;;
let mk_var v   = Inp(v)   ;; (* v has to be a string *)
let mk_const c = Const(c) ;; (* c has to be a boolean *)

(*
let deconstruct_bexp exp = match exp with
    And(_,_) -> mk_and
  | Or(_,_)  -> mk_or
  | Xor(_,_) -> mk_xor
  | Not(_)   -> mk_not
  | Inp(_)   -> mk_var
  | Const(_) -> mk_const ;;
*)
  
(*
let get_var_str exp = match exp with 
    (Inp s) -> s 
  | Xor(_,_) | Not _ | Or(_,_) | And(_,_) -> "" (*maybe we should raise exception here?*)
  | Const _ -> "" ;;
*)

let bin_funcs = [mk_and;mk_or;mk_xor] ;;

let choose_rand_bin_func _ = 
  List.nth bin_funcs (Random.int (List.length bin_funcs));;

let get_random_input lst = (List.nth lst (Random.int (List.length lst))) ;;

let do_with_prob f e exp prob = if (Random.float 1.0) < prob then
                                 f e
                              else exp ;;

let do_with_prob' exp inputs prob  = 
  let rn = Random.float 1.0 in
  if rn < prob then (
    match exp with 
     And(a,b) | Or(a,b) | Xor(a,b) -> (
       let func = ((choose_rand_bin_func ())) in
           func a b
      )
    |  Not(x) -> get_random_input inputs 
    |  Inp(x) -> mk_not exp
    |  Const(x) -> mk_not exp
    )
  else
    exp (*leave it unchanged *) ;;

let rec mutate_with_prob' exp inputs prob  = 
  let rn = Random.float 1.0 in
  let _ = (Printf.printf ">> rn is: %f\n" rn) in
    match exp with 
     And(a,b) | Or(a,b) | Xor(a,b) -> ( (*funcs with arity 2*)
       let func = ((choose_rand_bin_func ())) in
         if rn < prob then
           func (mutate_with_prob' a inputs prob) 
                (mutate_with_prob' b inputs prob)
         else (match exp with
             Not(_) | Const(_) | Inp(_) -> exp
           | And(a,b) -> And( (mutate_with_prob' a inputs prob),
                              (mutate_with_prob' b inputs prob) )
           | Or(a,b)  -> Or( (mutate_with_prob' a inputs prob),
                             (mutate_with_prob' b inputs prob) )
           | Xor(a,b) -> Xor( (mutate_with_prob' a inputs prob),
                              (mutate_with_prob' b inputs prob) )
         )
      )
    (*funcs with arity 1*)
    |  Not(x) -> if rn < prob then
                   (* 1/2 time get a new input, 1/2 time un-invert *)
                   if (Random.float 1.0) > 0.5 then
                     get_random_input inputs  (*could overly prune exp tree *)
                   else x (* else un-invert *)
                 else exp
    |  Inp(x) -> if rn < prob then 
                   (*1/2 time get a new input, 1/2 time invert *) 
                   if (Random.float 1.0) > 0.5 then
                     get_random_input inputs  (*could overly prune exp tree *)
                   else (mk_not exp)
                 else exp
    |  Const(x) -> if rn < prob then mk_not exp 
                   else exp;;
    
                              
let do_bin_with_prob f e1 e2 exp prob = if (Random.float 1.0) < prob then
                                 (f e1 e2)
                              else exp ;;

let num_ops = 3 ;;

type bop = AND | OR | XOR  ;; (*P_I = Primary Input *)

let bop_to_s op = match op with 
    AND -> "AND "
  | OR  -> "OR"
  | XOR -> "XOR" ;;

type uop = NEG | P_I ;;
let uop_to_s op = match op with
   NEG -> "NEG "
 | P_I -> "PrimaryInput " ;;

type unary_op  = uop * ( bexp -> bexp ) ;;
type binary_op = bop * ( bexp -> bexp -> bexp) ;;
type any_arity_func = UnaryFunc of unary_op  | 
                      BinaryFunc of binary_op ;;
(*
type unary_func = (bexp -> bexp ) ;;
type binary_func = (bexp->bexp->bexp) ;;
type any_arity = UnaryFunc of unary_func | BinaryFunc of binary_func ;;
*)




let unary_operations =  [UnaryFunc(P_I,(fun x -> x)); (*make type system happy*)
                         UnaryFunc(NEG,mk_not)];;

let binary_operations = [BinaryFunc(AND,mk_and);
                         BinaryFunc(OR,mk_or);
                         BinaryFunc(XOR,mk_xor) ] ;; 

(*
let decon_op op = match op with
    BinaryFunc(AND
 *)

let operations = unary_operations @ binary_operations;;
                      
let rec choose_rand_op _ = 
  List.nth operations (Random.int (List.length operations ))  ;;

let choose_rand_bin_op _ = 
  List.nth binary_operations (Random.int (List.length binary_operations));;


  

let choose_rand_unary_op _ = 
  List.nth unary_operations (Random.int (List.length unary_operations));;


let rec rand_bin_op (a_exp, b_exp) = match (Random.int num_ops) with
     0  -> And(a_exp, b_exp) 
  |  1  -> Or(a_exp, b_exp)
  |  2  -> Xor(a_exp, b_exp) 
  |  _  -> And(a_exp, b_exp) ;;

 let rec make_inps_lst num =
   let rec make_lst n lst = match n with 
     0 -> lst
   | _ -> make_lst (n-1) ((Inp (string_of_int n))::lst) in
   make_lst num [] ;;

let grow_rand_tree height inps = 
  let inputs = make_inps_lst inps in
  let rec grow_tree h =
      match h with 
      0 -> (get_random_input inputs) (*Primary inputs at 0 level *)
   |  _ -> match choose_rand_op ()  with
           UnaryFunc(op,func) ->  ( match op with
                P_I -> get_random_input inputs
              | NEG -> func (grow_tree (h-1)) )
         | BinaryFunc(op,func) ->  func (grow_tree (h-1)) (grow_tree (h-1)) in
   grow_tree height ;;


(* grow a tree  that's log2 #inputs high*)
let grow_rand_tree_log2 inputs = 
  let height = log2 (float_of_int  inputs) in
  grow_rand_tree (int_of_float height) inputs ;;
  
  

(*
  let rec eval exp env = match exp with
      Const x     -> x
    | And( x,y)   -> ( eval x env) *: ( eval y env) 
    | Or(x,y)     -> ( eval x env) +: ( eval y env)
    | Not(x)      -> n (eval x env) 
    | Xor(x,y)    -> xor (eval x env) (eval y env) 
    | Inp(x)      -> (Hashtbl.find env x)   ;;
*)

let list_by_pairs lst = 
  let rec list_pairs l = match l with
      [] -> []
    | a :: b :: xs -> (a,b) :: list_pairs xs
    | c :: []      -> [(c, List.hd lst)]  in
  list_pairs lst ;;

let rand_not exp = match Random.int 2 with
    0 -> exp
  | 1 -> Not exp
  | _ -> exp ;;



 let make_tree_from_list inps =
    let inps_lst = make_inps_lst inps in
    let rec make_ops l = match l with
       []        -> (Const F)
    |  a::b::[]  -> rand_bin_op(rand_not a, rand_not b) 
    |  a::b::xs  -> rand_bin_op(rand_bin_op(rand_not a, rand_not b), (make_ops xs) )
    |  c :: []   -> (rand_not c) in
    make_ops inps_lst ;;


 let get_random_input lst = (List.nth lst (Random.int (List.length lst))) ;;


let cross exp1 exp2 = 
  let depth_exp1 = op_count exp1 in
  let depth_exp2 = op_count exp2 in
  let cross_depth1 = Random.int depth_exp1 in
  let cross_depth2 = Random.int depth_exp2 in 
  let rec goto_depth exp depth = match depth with
    0 -> exp
  (*| _ as n  when depth = n -> exp*)
  | _ -> match exp with
          Const(_) -> exp
        | Inp(_)   -> exp (*shouldn't we swap at this level too? *)
        | And(x,y) | Or(x,y) | Xor(x,y) -> if( Random.float 1.0) > 0.5 then
                                       goto_depth x (depth-1) 
                                     else
                                       goto_depth y (depth-1)
       | Not(x) -> goto_depth x (depth-1) in

  let crossed = ref false in (*HERE BE DRAGONS!  BEWARE OF STATE!!!*)
  (* can we thread the state instead? *)
  (*a kind of one-shot, first time will be exp2, after that exp1 *)
  let select_exp exp1 exp2  = (Printf.printf "crossed is: %b\n" !crossed);
                              if (!crossed)  then exp1 
                              else ( crossed := true; exp2)  in
  (*Problem with this scheme is that the first terminal we hit will always be 
    the one that gets replaced, so most likely a Cosnst, Inp *)
  

  let rec goto_depth_rep exp exp' depth = 
    match depth with
    0 -> (match exp with 
           Const _  -> select_exp exp exp' 
         | Inp _    -> select_exp exp exp' 
         | Not x    -> Not(select_exp exp exp' )
         | And(x,y) -> select_exp exp exp' (*if(Random.float 1.0) > 0.5 then
                         And(exp',y)
                       else And(x,exp') *)
         | Or(x,y)  -> select_exp exp exp'  (*if(Random.float 1.0) > 0.5 then
                         Or(exp',y)
                       else Or(x,exp') *)
         | Xor(x,y) -> select_exp exp exp'  (*if(Random.float 1.0) > 0.5 then
                         Xor(exp',y)
                       else Xor(x,exp') *) )
  | _ -> match exp with 
           Const _ -> exp 
         | Not x   -> (goto_depth_rep x exp' (depth-1)) 
         | Inp _   -> exp 
         | And(x,y)-> And( (goto_depth_rep x exp' (depth-1)),
                           (goto_depth_rep y exp' (depth-1)) )
         | Or(x,y) -> Or ( (goto_depth_rep x exp' (depth-1)),
                           (goto_depth_rep y exp' (depth-1)) )
         | Xor(x,y)-> Xor( (goto_depth_rep x exp' (depth-1)),
                           (goto_depth_rep y exp' (depth-1)) ) in

  let exp1' = goto_depth exp1 cross_depth1 in
  let exp2' = goto_depth_rep exp2 exp1' cross_depth2 in
  (*let exp2' = branch_rep exp2 exp1' 0  in *)
  let _ = Printf.printf "cross_depth2 is %d\n" cross_depth2  in
  exp1',exp2';;

let rec incr_bin lst =
  let rec incr l p = match l with
     [] -> []
   | x :: xs -> (x ^: p) :: (incr xs (x *: p)) in
   incr lst T ;;        

let rec count_bin lst f = 
  let max_count = int_of_float(2.0 ** float_of_int(List.length lst)) in
  let rec count lst n = match n with
      0 -> lst
    | _ ->  (f lst) ; count (incr_bin lst) (n-1) in
    count lst max_count ;;

  
let rec eval_all_inputs exp subfn v ins  =  match ins with
    [] -> subfn v
  | i::inps -> let v' t q = if q = i then t else v(q) in
               (eval_all_inputs exp subfn (v' F) inps)  *: 
               (eval_all_inputs exp subfn (v' T) inps)  ;;

(* evaluate an expression will all input combinations*)
let rec do_exp_eval exp = 
  let inps = uniqify(get_inputs exp) in
  let _ = List.iter ( fun x -> Printf.printf "Inp: %s\n" (expr_to_str x)) inps in
  let do_assign v = 
    (*let lis = List.map (fun x -> b_to_s(v x)) inps *)
    let ans = (eval exp v ) in
    (Printf.printf "Ans: %s\n" (b_to_s ans));T in
  eval_all_inputs exp do_assign (fun x -> F) inps ;;

let rec print_str_lst lst = match lst with
    [] -> () (*Printf.printf "\n"*)
  | x::xs -> Printf.printf "%s" x; print_str_lst xs ;;

let rec print_expr_lst lst = match lst with
    [] -> () (*Printf.printf "\n"*)
  | x::xs -> Printf.printf "%s" (expr_to_str x); print_expr_lst xs ;;

let rec create_truth_tabl lst = 
  let tabl = Hashtbl.create 255  in
  let rec iter lst' = match lst' with 
     [] -> tabl
   | x::xs -> (Hashtbl.add tabl (fst x) (snd x)) ; iter xs in
  iter lst ;;

(* evaluate the expression with every combination of inputs *)
let  do_exp_eval_comp exp tabl  = 
  (*let counter (cond,v) = if (cond = true) then (cond,v+1) else (cond,v) in*)
  (*TODO: mutable STATE on following line: here be DRAGONS *)
  let count = ref 0 in 
  (*TODO: need to uniqify the inps from get_inputs *)
  let inps = (List.sort (fun a b -> compare a b ) (uniqify (get_inputs exp))) in
  let _ = List.iter ( fun x -> Printf.printf "Inp: %s\n" (expr_to_str x)) inps in
  let do_assign v   = 
    let lis = List.map (fun x -> (v x)) inps  in
    let ans = (eval exp v ) in
    let gold_ans = Hashtbl.find tabl lis in
    let mismatch = not (ans = gold_ans) in
    let _ = (if mismatch then (incr count) else () ) in
    (Printf.printf " | %s %d\n" (b_to_s ans) !count );if !count = 0 then T  else F in
  let _ = eval_all_inputs exp do_assign (fun x -> F) inps in
  !count;;

(*let rec do_gp max_iter inputs = match max_iter with *)

