(*
module Logic :
  sig
    exception VarNotDefined
    type boolean = T | F
    type variable = Name of string | NameVal of string * boolean
    type 'a optional = Some of 'a | None
    val to_bool : boolean -> bool
    val b_to_s : boolean -> string
    type bexp =
        Const of boolean
      | Var of string
      | And of bexp * bexp
      | Or of bexp * bexp
      | Not of bexp
      | Xor of bexp * bexp
    val and_ : boolean -> boolean -> boolean
    val or_ : boolean -> boolean -> boolean
    val n : boolean -> boolean
    val xor : boolean -> boolean -> boolean
    val ( *: ) : boolean -> boolean -> boolean
    val ( +: ) : boolean -> boolean -> boolean
    val ( ^: ) : boolean -> boolean -> boolean
    val ( !: ) : boolean -> boolean
    val expr_to_str : bexp -> string
    val reduce : bexp -> bexp
    val demorganize : bexp -> bexp
    val eval : bexp -> (string, boolean) Hashtbl.t -> boolean
    val literal_count : bexp -> int
    val op_count : bexp -> int
  end
      = struct
      *)
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


  type bexp = Const of boolean 
    |  Var of string 
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
    | Var(x)      ->  " " ^ x ^ " "

  let rec reduce exp = 
     (*let _ = Printf.printf "reduce top: exp is: %s\n" (expr_to_str exp ) in*)
     match exp with
      Const x                                         -> exp
    | Var x                                           -> exp 
    | Not(Const F)           -> Const T
    | Not(Const T)           -> Const F
    | Not(Var x)             -> exp 
    | Not(Not(Var x))        -> (Var x) 
    | Not(x)                 -> (reduce (Not(reduce x)))
    | And(x,y) when x = y    -> (reduce x)
    | And(Const T, Const T)                           -> Const T
    | And(Const F, _) | And(_, Const F)               -> Const F
    | And(Var x, Var y)      -> exp
    | And( Not(Var x),  y)   -> And(Not(Var x), reduce y)
    | And( x, Not(Var y))    -> And(reduce x, Not(Var y))
    | And(Var x, y)          -> And(Var x, reduce y)
    | And( x, Var y)         -> And(reduce x, Var y)
    | And(x,y)               -> (*Printf.printf "reduce: And(x,y): \n";*) (reduce(And(reduce x,reduce y)))
    | Or(x,y)  when x=y      -> (reduce x)
    | Or(Const T ,_) | Or(_,Const T)                  -> Const T
    | Or(Const F, Const F)                            -> Const F
    | Or(Var x, Var y)       -> exp
    | Or( Not(Var x),  y) ->    Or(Not(Var x), reduce y)
    | Or( x, Not(Var y))     -> Or(reduce x, Not(Var y))
    | Or(Var x, y)           -> Or(Var x, reduce y)
    | Or( x, Var y)          -> Or(reduce x, Var y)
    | Or(x,y)                -> (reduce (Or(reduce x,reduce y)))
    | Xor(Const T,Const T) | Xor(Const F ,Const F)    -> Const F
    | Xor(Const T,Const F) | Xor(Const F, Const T)    -> Const T 
    | Xor(Var x, Var y)      -> exp
    | Xor( Not(Var x),  y) ->    Xor(Not(Var x), reduce y)
    | Xor( x, Not(Var y))     -> Xor(reduce x, Not(Var y))
    | Xor(Var x, y)          -> Xor(Var x, reduce y)
    | Xor( x, Var y)         -> Xor(reduce x, Var y)
    | Xor(x,y)  -> (reduce (Xor(reduce x, reduce y)))

      ;;

  let rec demorganize exp = match exp with 
    | And(Not( x), Not( y) ) -> Not( Or( x,y)) 
    | Or( Not x, Not y)      -> Not( And( x, y) ) 
    | Not( Or(x,y) )         -> And(Not x, Not y)
    | Not( And(x,y) )        -> Or(Not x, Not y)
    | _ -> exp ;;

  let rec eval exp env = match exp with
      Const x     -> x
    | And( x,y)   -> ( eval x env) *: ( eval y env) 
    | Or(x,y)     -> ( eval x env) +: ( eval y env)
    | Not(x)      -> n (eval x env) 
    | Xor(x,y)    -> xor (eval x env) (eval y env) 
    | Var(x)      -> (Hashtbl.find env x)   ;;

  let rec literal_count exp = 
    let rec count_literals exp' lc = match exp' with
    | Var(x) -> 1
    | Const(x) -> 0
    | And(x,y) | Or(x,y) | Xor(x,y) -> (lc + ( count_literals x lc) + ( count_literals y lc) ) 
    | Not(x) -> (lc + ( count_literals x lc)) in 
      count_literals exp 0  ;;

  let rec op_count exp = 
    let rec count_ops exp' oc = match exp' with
    | Var(_) | Const(_) -> Printf.printf "op_count: Var/Const"; 0
    | Not(x) -> Printf.printf "op_count: Not(x)n"; 0 (*( count_ops x oc) not counting Not as op*)
    | And(x,y) | Or(x,y) | Xor(x,y) -> Printf.printf "And/Or/Xor\n";(1 + (count_ops x oc) + (count_ops y oc)) in
      count_ops exp 0 
    ;;
(*

end ;;
*)
       
(*
open Logic;;
open Printf;;
open OUnit ;;
(*
let x = Var("x", Some(F)) ;;
let y = Var("y", Some(T));;
*)
(*let my_env = Hashtbl.create 225;;*)
let expr = Xor(Const(F),Const(T));;

printf "%s\n" ( expr_to_str expr );;

let expr2 = And(expr,Not(Const(F)));;
printf "%s\n" ( expr_to_str expr2 );;
let test_construct _ = 
  assert_equal  (And(Xor(Const F, Const T), Not(Const F)))   expr2 ;;

let test_expr_to_str _ =
  assert_equal " (F ^ T) " (expr_to_str expr)

let suite = "Logic test suite" >::: ["test_construct" >:: test_construct;
             "test_expr_to_str" >:: test_expr_to_str] ;;


let _ =
  run_test_tt ~verbose:true suite ;;
(*
printf "\n";;
printf "expr2: %s\n" ( expr_to_str expr2) ;;
let reduced_exp = reduce expr2 ;;
printf "%s\n" ( expr_to_str  reduced_exp) ;;
let anded = And(expr,expr) ;;
let reduced_anded = (reduce anded) ;;
printf "%s\n" ( expr_to_str reduced_anded) ;;
let y = Var("y") ;;
let x = Var("x") ;;
let z = Var("z") ;;
Hashtbl.add my_env "y" F ;;
Hashtbl.add my_env "x" F ;;
Hashtbl.add my_env "z" T ;;
let x_and_y = And( z, Or((Not(z)),And(x,y))) ;;
printf "num ops: %d\n" (op_count x_and_y) ;;
(*printf "value is: %s\n" ( eval x_and_y (*my_env*)) ;;*)
printf "value is: %s\n" ( b_to_s (eval x_and_y my_env) ) ;;
printf "x_and_y is %s\n" ( expr_to_str ( reduce x_and_y )) ;;
let not_x_and_not_y = And(Not(Var "x"), Not(Var "y") ) ;;
printf "demorganize not_x_and_not_y %s\n" ( expr_to_str (demorganize not_x_and_not_y )) ;;
let count =   literal_count ( And(Const T,x_and_y) )  ;;
printf "%d\n" ( count ) ;; 
*)
*)
