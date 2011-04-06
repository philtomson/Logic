  exception VarNotDefined ;;
  type boolean = T | F ;;
  type variable = Name of string | NameVal of string*boolean ;;
  type 'a optional  = Some of 'a | None ;;

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
    |  Var of string  (* TODO: perhaps rename Var to Input *)
    |  Bop of bop * bexp * bexp
    |  Not of bexp
  and bop = And | Or | Xor
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

  let bop_to_func op = match op with
    | And -> and_
    | Or  -> or_
    | Xor -> xor 

  let ( *: ) x y =  and_ x y ;;
  let ( +: ) x y =  or_ x y ;;
  let ( ^: ) x y =  xor x y ;;
  let ( !: ) x   =  n x ;; 

  let op_to_str op = match op with
    | And -> " * "
    | Or  -> " + "
    | Xor -> " ^ " 

  let rec expr_to_str exp = match exp with
    | Const x     ->  (b_to_s x ) 
    | Bop(op,x,y) ->  " (" ^ (expr_to_str x) ^ (op_to_str op) ^ 
                             (expr_to_str y) ^ ") "
    | Not(x)      ->  " !"^  (expr_to_str x) ^ " " 
    | Var(x)      ->  " " ^ x ^ " "

  let rec reduce exp = 
     match exp with
      Const x                -> exp
    | Var x                  -> exp 
    | Not(Const F)           -> Const T
    | Not(Const T)           -> Const F
    | Not(Var x)             -> exp 
    | Not(Not(Var x))        -> (Var x) 
    | Not(x)                 -> (reduce (Not(reduce x)))
    | Bop(And, x,y) when x = y  -> (reduce x)
    | Bop(Or, x,y)  when x = y  -> (reduce x) 
    | Bop(And, Const T, Const T)                  -> Const T
    | Bop(And, Const F, _) | Bop(And, _, Const F) -> Const F
    | Bop(Or,  Const T, _) | Bop(Or,  _, Const T) -> Const T
    | Bop(Or,  Const F, Const F)                  -> Const F
    | Bop(Xor, Const T,Const T) | Bop(Xor, Const F, Const F)  -> Const F
    | Bop(Xor, Const T,Const F) | Bop(Xor, Const F, Const T)  -> Const T 
    | Bop(_, Var x, Var y)   -> exp
    | Bop(_ as op, Not(Var x),  y) -> Bop(op, Not(Var x), reduce y)
    | Bop(_ as op, x, Not(Var y))  -> Bop(op, reduce x, Not(Var y))
    | Bop(_ as op, Var x, y)       -> Bop(op, Var x, reduce y)
    | Bop(_ as op, x, Var y)       -> Bop(op, reduce x, Var y)
    | Bop(_ as op, x,y)            -> (reduce (Bop(op, reduce x,reduce y)))
      ;;

  let rec demorganize exp = match exp with 
    | Bop(And, Not x, Not y)   -> Not(Bop(Or,  x, y)) 
    | Bop(Or,  Not x, Not y)   -> Not(Bop(And, x, y)) 
    | Not( Bop(Or, x,y) )      -> Bop(And, Not x, Not y)
    | Not( Bop(And,x,y) )      -> Bop(Or, Not x, Not y)
    | _ -> exp ;;


  let rec eval exp env = match exp with
      Const x     -> x
    | Bop(op,x,y) -> (bop_to_func op) ( eval x env) ( eval y env) 
    | Not(x)      -> n (eval x env) 
    | Var(x)      -> (Hashtbl.find env x)   ;;

  let rec get_inputs exp = match exp with 
      Const x     -> []
    | Var x       -> [exp]
    | Bop(_, x,y) -> (get_inputs x) @ (get_inputs y)
    | Not x       -> get_inputs x ;;

  let rec literal_count exp = 
    let rec count_literals exp' lc = match exp' with
    | Var(x)     -> 1
    | Const(x)   -> 0
    | Bop(_,x,y) -> (lc + ( count_literals x lc) + ( count_literals y lc) ) 
    | Not(x)     -> (lc + ( count_literals x lc)) in 
      count_literals exp 0  ;;

  let rec op_count exp = 
    let rec count_ops exp' oc = match exp' with
    | Var(_) | Const(_) ->  0
    | Not(x)     ->  0 (*( count_ops x oc) not counting Not as op*)
    | Bop(_,x,y) -> (1 + (count_ops x oc) + (count_ops y oc)) in
      count_ops exp 0 
    ;;


let mk_and a b = Bop(And,a,b) ;;
let mk_or  a b = Bop(Or,a,b)  ;;
let mk_xor a b = Bop(Xor,a,b) ;;
let mk_not a   = Not(a)   ;;
let mk_var v   = Var(v)   ;; (* v has to be a string *)
let mk_const c = Const(c) ;; (* c has to be a boolean *)

(*
let deconstruct_bexp exp = match exp with
    And(_,_) -> mk_and
  | Or(_,_)  -> mk_or
  | Xor(_,_) -> mk_xor
  | Not(_)   -> mk_not
  | Var(_)   -> mk_var
  | Const(_) -> mk_const ;;
*)
  
(*
let get_var_str exp = match exp with 
    (Var s) -> s 
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
     Bop(_,a,b)  -> (
       let func = ((choose_rand_bin_func ())) in
           func a b
      )
    |  Not(x) -> get_random_input inputs 
    |  Var(x) -> mk_not exp
    |  Const(x) -> mk_not exp
    )
  else
    exp (*leave it unchanged *) ;;

let rec mutate_with_prob' exp inputs prob  = 
  let rn = Random.float 1.0 in
  let _ = (Printf.printf ">> rn is: %f\n" rn) in
    match exp with 
     Bop(_,a,b)  -> ( (*funcs with arity 2*)
       let func = ((choose_rand_bin_func ())) in
         if rn < prob then
           func (mutate_with_prob' a inputs prob) 
                (mutate_with_prob' b inputs prob)
         else (match exp with
             Not(_) | Const(_) | Var(_) -> exp
           | Bop(_,a,b) -> Bop(And, (mutate_with_prob' a inputs prob),
                                      (mutate_with_prob' b inputs prob))
         )
      )
    (*funcs with arity 1*)
    |  Not(x) -> if rn < prob then
                   (* 1/2 time get a new input, 1/2 time un-invert *)
                   if (Random.float 1.0) > 0.5 then
                     get_random_input inputs  (*could overly prune exp tree *)
                   else x (* else un-invert *)
                 else exp
    |  Var(x) -> if rn < prob then 
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

(*
type bop = AND | OR | XOR  ;; (*P_I = Primary Input *)
*)

let bop_to_s op = match op with 
    And -> "AND "
  | Or  -> "OR"
  | Xor -> "XOR" ;;

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

let binary_operations = [BinaryFunc(And,mk_and);
                         BinaryFunc(Or,mk_or);
                         BinaryFunc(Xor,mk_xor) ] ;; 

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
     0  -> Bop(And,a_exp, b_exp) 
  |  1  -> Bop(Or, a_exp, b_exp)
  |  2  -> Bop(Xor,a_exp, b_exp) 
  |  _  -> Bop(And,a_exp, b_exp) ;;

let grow_rand_tree height inputs = 
  let rec grow_tree h =
      match h with 
      0 -> (get_random_input inputs) (*Primary inputs at 0 level *)
   |  _ -> match choose_rand_op ()  with
           UnaryFunc(op,func) ->  ( match op with
                P_I -> get_random_input inputs
              | NEG -> func (grow_tree (h-1)) )
         | BinaryFunc(op,func) ->  func (grow_tree (h-1)) (grow_tree (h-1)) in
   grow_tree height ;;

(*
  let rec eval exp env = match exp with
      Const x     -> x
    | And( x,y)   -> ( eval x env) *: ( eval y env) 
    | Or(x,y)     -> ( eval x env) +: ( eval y env)
    | Not(x)      -> n (eval x env) 
    | Xor(x,y)    -> xor (eval x env) (eval y env) 
    | Var(x)      -> (Hashtbl.find env x)   ;;
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

 let make_tree_from_list lst =
    let rec make_ops l = match l with
       []        -> (Const F)
    |  a::b::[]  -> rand_bin_op(rand_not a, rand_not b) 
    |  a::b::xs  -> rand_bin_op(rand_bin_op(rand_not a, rand_not b), (make_ops xs) )
    |  c :: []   -> (rand_not c) in
    make_ops lst ;;


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
          Const(_)   -> exp
        | Var(_)     -> exp (*shouldn't we swap at this level too? *)
        | Bop(_,x,y) -> if( Random.float 1.0) > 0.5 then
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
    the one that gets replaced, so most likely a Cosnst, Var *)
  

  let rec goto_depth_rep exp exp' depth = 
    match depth with
    0 -> (match exp with 
           Const _  -> select_exp exp exp' 
         | Var _    -> select_exp exp exp' 
         | Not x    -> Not(select_exp exp exp' )
         | Bop(_ ,x,y) -> select_exp exp exp' (*if(Random.float 1.0) > 0.5 then
                         Bop(op,exp',y)
                       else Bop(op,x,exp') *) )
  | _ -> match exp with 
           Const _ -> exp 
         | Not x   -> (goto_depth_rep x exp' (depth-1)) 
         | Var _   -> exp 
         | Bop(_ as op,x,y)-> Bop(op, (goto_depth_rep x exp' (depth-1)),
                                      (goto_depth_rep y exp' (depth-1))) in

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

  
