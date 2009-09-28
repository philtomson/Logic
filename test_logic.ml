open Logic;;
open Printf;;
open OUnit ;;
(*
let x = Var("x", Some(F)) ;;
let y = Var("y", Some(T));;
*)
let my_env = Hashtbl.create 225;;
let expr = Xor(Const(F),Const(T));;

printf "%s\n" ( expr_to_str expr );;

let expr2 = And(expr,Not(Const(F)));;
printf "%s\n" ( expr_to_str expr2 );;
let test_construct _ = 
  assert_equal  (And(Xor(Const F, Const T), Not(Const F)))   expr2 ;;

let test_expr_to_str _ =
  assert_equal " (F ^ T) " (expr_to_str expr) ;;


printf "expr2: \n%s\n" ( expr_to_str expr2) ;;

let reduced_exp = reduce expr2 ;;
printf "%s\n" ( expr_to_str  reduced_exp) ;;


let anded = And(expr,expr) ;;
let reduced_anded = (reduce anded) ;;
printf "%s\n" ( expr_to_str reduced_anded) ;;

let test_reduce _ = 
  assert_equal (Const T) reduced_exp ;
  assert_equal (Const T) reduced_anded ;;

let y = Var("y") ;;
let x = Var("x") ;;
let z = Var("z") ;;
Hashtbl.add my_env "y" F ;;
Hashtbl.add my_env "x" F ;;
Hashtbl.add my_env "z" T ;;
let x_and_y = And( z, Or((Not(z)),And(x,y))) ;;
printf "num ops: %d\n" (op_count x_and_y) ;;

let test_op_count _ = 
  assert_equal 3 (op_count x_and_y) ;;
(*
(*printf "value is: %s\n" ( eval x_and_y (*my_env*)) ;;*)
printf "value is: %s\n" ( b_to_s (eval x_and_y my_env) ) ;;
printf "x_and_y is %s\n" ( expr_to_str ( reduce x_and_y )) ;;
let not_x_and_not_y = And(Not(Var "x"), Not(Var "y") ) ;;
printf "demorganize not_x_and_not_y %s\n" ( expr_to_str (demorganize not_x_and_not_y )) ;;
let count =   literal_count ( And(Const T,x_and_y) )  ;;
printf "%d\n" ( count ) ;; 
*)

let suite = "Logic test suite" >::: [
             "test_construct" >:: test_construct;
             "test_expr_to_str" >:: test_expr_to_str;
             "test_reduce"      >:: test_reduce
                                    ] ;;

let _ =
  run_test_tt ~verbose:true suite ;;