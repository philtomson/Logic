module Logic = struct
  type boolean = T | F ;;
  type variable = Name of string | NameVal of string*boolean ;;
  type 'a optional  = Some of 'a | None ;;

  let to_bool v = match v with 
      T -> true
    | F -> false ;;

  let b_to_s v = match v with
      T  -> "T"
    | F  -> "F" ;;


  type bexp = Const of boolean 
   (* |  Var of string *)
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

  let rec reduce exp = match exp with
      Const x                                         -> exp
    | And(Const T, Const T)                           -> Const T
    | And(Const F, _) | And(_, Const F)               -> Const F
    | And(x,y)  -> (reduce (And(reduce x,reduce y)))
    | Or(Const T ,_) | Or(_,Const T)                  -> Const T
    | Or(Const F, Const F)                            -> Const F
    | Or(x,y)   -> (reduce (Or(reduce x,reduce y)))
    | Xor(Const T,Const T) | Xor(Const F ,Const F)    -> Const F
    | Xor(Const T,Const F) | Xor(Const F, Const T)    -> Const T 
    | Xor(x,y)  -> (reduce (Xor(reduce x, reduce y)))
    | Not(Const F) -> Const T
    | Not(Const T) -> Const F
    | Not(x)    -> (reduce (Not(reduce x)))
                    ;;

  let rec eval exp  = match exp with
      Const x     -> x
    | And( x,y)   -> ( eval x ) *: ( eval y) 
    | Or(x,y)     -> ( eval x ) +: ( eval y)
    | Not(x)      -> n (eval x) 
    | Xor(x,y)    -> xor (eval x) (eval y) 
    (*
    | Var(x,y)    -> (match y with
                       None -> F
                     | Some(x') -> x')    ;;
    *)
                     
  let rec expr_to_str exp = match exp with
    | Const x     ->  (b_to_s x ) 
    | And(x,y)    ->  " (" ^ (expr_to_str x) ^ " * " ^ 
                             (expr_to_str y) ^ ") "
    | Or(x,y)     ->  " (" ^ (expr_to_str x) ^ " + " ^ 
                             (expr_to_str y) ^ ") "
    | Xor(x,y)    ->  " (" ^ (expr_to_str x) ^ " ^ " ^ 
                             (expr_to_str y) ^ ") "
    | Not(x)      ->  " !"^  (expr_to_str x) ^ " " 
    (*| Var(x,y)    ->  x *)

                      

end ;;
       
open Logic;;
open Printf;;
(*
let x = Var("x", Some(F)) ;;
let y = Var("y", Some(T));;
*)
let expr = Xor(Const(F),Const(T));;
printf "%s\n" ( expr_to_str expr );;
let expr2 = And(expr,Not(Const(F)));;
printf "\n";;
printf "expr2: %s\n" ( expr_to_str expr2) ;;
let reduced_exp = reduce expr2 ;;
printf "%s\n" ( expr_to_str  reduced_exp) ;;

