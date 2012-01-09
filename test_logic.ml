open Logic;;
open Printf;;
open OUnit ;;
open Fsm;;

(*
let x = Var("x", Some(F)) ;;
let y = Var("y", Some(T));;
*)
let expr = Bop(Xor,Const(F),Const(T));;

printf "%s\n" ( expr_to_str expr );;
Random.self_init  ;;

let expr2 = Bop(And, expr, Not(Const(F)));;
printf "%s\n" ( expr_to_str expr2 );;
let test_construct _ = 
  assert_equal  (Bop(And,Bop(Xor, Const F, Const T), Not(Const F)))   expr2 ;;

let test_expr_to_str _ =
  assert_equal " (F ^ T) " (expr_to_str expr) ;;


printf "expr2: \n%s\n" ( expr_to_str expr2) ;;

let reduced_exp = reduce expr2 ;;
printf "%s\n" ( expr_to_str  reduced_exp) ;;


let anded = Bop(And,expr,expr) ;;
let reduced_anded = (reduce anded) ;;
printf "%s\n" ( expr_to_str reduced_anded) ;;

let test_reduce _ = 
  assert_equal (Const T) reduced_exp ;
  assert_equal (Const T) reduced_anded ;;

let y = Var({name = "y"; value = F}) ;;
let x = Var({name = "x"; value = F}) ;;
let z = Var({name = "z"; value = T}) ;;
(*
Hashtbl.add my_env y F ;;
Hashtbl.add my_env x F ;;
Hashtbl.add my_env z T ;;
*)
let _ = 
  assign x F  ;
  assign y F  ;
  assign z T  ;;

let x_and_y = Bop(And, z, Bop(Or,(Not(z)),Bop(And,x,y))) ;;
printf "num ops: %d\n" (op_count x_and_y) ;;

  
let rec iter_bop_ttbl lst op = match lst with  (* test binary operators *)
    [] -> ()
  | x::xs -> 
    (
      let ins = fst x in
      let res = snd x in
      let i1  = fst ins in
      let i2  = snd ins in
      let expr = op (Const i1) (Const i2) in
      assert_equal  (eval expr ) (res) );  iter_bop_ttbl xs op ; ()  ;;

let test_not _ =
  assert_equal ( eval (Not(Const T)) ) F ;
  assert_equal ( eval (Not(Const F)) ) T ;;

let test_and _ = 
  let inputs = [((F,F),F);
                ((F,T),F);
                ((T,F),F);
                ((T,T),T)] in
  iter_bop_ttbl inputs mk_and ;;

let test_or _ = 
  let inputs = [((F,F),F);
                ((F,T),T);
                ((T,F),T);
                ((T,T),T)] in
  iter_bop_ttbl inputs mk_or ;;

let test_xor _ = 
  let inputs = [((F,F),F);
                ((F,T),T);
                ((T,F),T);
                ((T,T),F)] in
  iter_bop_ttbl inputs mk_xor ;;

(*
  let rec iter lst = match lst with
    [] -> ()
  | x::xs -> 
    (
      let ins = fst x in
      let res = snd x in
      let i1  = fst ins in
      let i2  = snd ins in
      let expr = Bop(And,Const i1, Const i2) in
      assert_equal (eval expr my_env) res );  iter xs ; ()  in
  iter inputs ;;
  *)


let test_op_count _ = 
  assert_equal 3 (op_count x_and_y) ;;
(*
(*printf "value is: %s\n" ( eval x_and_y (*my_env*)) ;;*)
printf "value is: %s\n" ( b_to_s (eval x_and_y my_env) ) ;;
printf "x_and_y is %s\n" ( expr_to_str ( reduce x_and_y )) ;;
*)
let not_x_and_not_y = Bop(And,Not x , Not y ) ;;
printf "demorganize not_x_and_not_y %s\n" ( expr_to_str (demorganize not_x_and_not_y )) ;;
let count =   literal_count ( Bop(And,Const T,x_and_y) )  ;;
printf "%d\n" ( count ) ;; 
(**)
let test_demorganize _ = 
  let not_x_or_y = demorganize not_x_and_not_y in
  assert_equal (demorganize not_x_or_y) (Bop(And, Not x, Not y));
  assert_equal (not_x_and_not_y) (demorganize ( demorganize not_x_and_not_y));;
  (*assert_equal (demorganize not_x_or_y) (Not (Or (Var "x", Var "y")))*)
  (*assert_equal (demorganize not_x_or_y) (Not( Bop(Or,x, y))) ;;*)

let inputs = [Var ({name="a"; value=F}); 
              Var ({name="b"; value=F}); 
              Var ({name="c"; value=F});
              Var ({name="d"; value=F});
              Var ({name="e"; value=F})];;

let op_tree = make_tree_from_list inputs;;
printf "op_tree: \n%s\n" ( (expr_to_str ( op_tree))) ;;
let op_tree1 = grow_rand_tree 6 inputs  ;;
printf "op_tree1: \n%s\n" ( (expr_to_str ( op_tree1))) ;;
(***********************************************************
 * for some reason this section of commented code throws the 
 * following exception:
 * Fatal error: exception Invalid_argument("Random.int")
printf "mutated op_tree1:\n %s\n" (( expr_to_str ( mutate_with_prob' op_tree1 inputs 0.1)));;

Random.self_init ;;
let op_tree_inputs = get_inputs op_tree ;;
let _ = List.iter ( fun x -> (Printf.printf "Input: %s\n" (expr_to_str x)); () ) op_tree_inputs ;;
let parent1,pruned_tree = (cross op_tree op_tree1) ;;
printf "Parent1 (from op_tree): \n%s\n" (( expr_to_str parent1)) ;;
printf "crossed op_tree1: \n%s\n" (( expr_to_str pruned_tree)) ;;
printf "op_tree1 depth is: %d\n" (op_count op_tree1 ) ;;
*************************************************************)

count_bin [F;F;F;F] ( fun lst -> print_bool_lst lst ) ;;
                         


let op_tree2 = grow_rand_tree 1 inputs  ;;
printf "op_tree2: \n%s\n" ( (expr_to_str ( op_tree2))) ;;
Random.self_init ;;
let op_tree3 = grow_rand_tree 2 inputs  ;;
printf "op_tree3: \n%s\n" ( (expr_to_str ( op_tree3))) ;;

(*********************************************************
 * FSM testing *******************************************
*)

(* inputs *)
let full         = Var({name ="full"; value  = F});;
let ten_minutes  = Var({name = "ten_minutes"; value = F});;
let empty        = Var({name = "empty"; value = F});;
let five_minutes = Var({name = "five_minutes"; value =F});;



let _ = 
  assign full         F ;
  assign ten_minutes  F ;
  assign empty        F ;
  assign five_minutes F ;;

(* outputs *)
let water_on     = Var({name = "water_on";    value = F});;
let agitate      = Var({name = "agitate";     value = F});;
let drain        = Var({name = "drain"  ;     value = F});;
let start_timer  = Var({name = "start_timer"; value = F});;
let motor_on     = Var({name = "motor_on";    value = F});;
let washed       = Var({name = "washed";    value = F});;
let soap         = Var({name = "soap";        value = F});;

let reset_actions = 
  assign water_on      F;
  assign agitate       F;
  assign drain         F;
  assign start_timer   F;
  assign motor_on      F;;

module WashStates = 
  struct
   type t =  START | FILL | WASH | DRAIN |  RINSE | SPIN | STOP
   deriving(Show, Enum)
     
   let start_state = START

  end 

module LogicExp = 
  struct
    type t     = Logic.bexp
    type var_t = Logic.variable

    let eval_exp exp = Logic.to_bool (Logic.eval exp)

    let var_to_s     = Logic.var_to_s

  end


module WashFSM = FSM(WashStates)(LogicExp) 

open WashStates

  (* CS,     PREDICATE,             NS,    ACTIONs *)
let my_fsm = [
  (START, Const(T),                 FILL, [(water_on,   T); 
                                           (soap,       T)]);
  (FILL,  Bop(And,full,soap),       WASH, [(water_on,   F);
                                           (agitate,    T);
                                           (washed,     T);
                                           (start_timer,T)]);
  (WASH,  ten_minutes,              DRAIN,[(agitate,    F);
                                           (start_timer,F); 
                                           (empty,      T)]); 
  (DRAIN, Bop(And,empty,soap),      FILL, [(drain,      F); 
                                           (soap,       F);
                                           (water_on,   T)] );
  (FILL,  Bop(And,full,Not(soap)),  RINSE,[(water_on,   F); 
                                           (soap,       F);
                                           (empty,      F);
                                           (agitate,    T)]);
  (RINSE, ten_minutes,              DRAIN, [(agitate,   F);
                                            (empty,     T)] );
  (DRAIN, Bop(And,empty,Not(soap)), SPIN,  [(motor_on,  T);
                                            (start_timer,T)]);
  (SPIN,  five_minutes,             STOP,  [(water_on,  F);
                                            (drain,     F);
                                            (start_timer,F);
                                            (motor_on,  F)]);
  (STOP,  Const(T),                 STOP,  [(motor_on,  F)]);
];; 


let st_table, current_state = WashFSM.create my_fsm in

let _ = assign full T in
let current_state = WashFSM.eval_fsm st_table current_state  in
let _ = assign ten_minutes T in
let current_state = WashFSM.eval_fsm st_table current_state  in
let current_state = WashFSM.eval_fsm st_table current_state  in
let _ = (assign ten_minutes F);(assign empty T) in
let current_state = WashFSM.eval_fsm st_table current_state  in

let _ = assign five_minutes T in
let current_state = WashFSM.eval_fsm st_table current_state  in
let _ = assign five_minutes F in
let _ = assign ten_minutes T in
let current_state = WashFSM.eval_fsm st_table current_state  in
let current_state = WashFSM.eval_fsm st_table current_state  in
let _ = assign five_minutes T in
let _ = WashFSM.eval_fsm st_table current_state  in
let _ = WashFSM.get_inputs st_table in
let outs = WashFSM.get_outputs st_table in

print_endline ( WashFSM.enum_states) ;; 


(*
module BoolExp = 
  struct
    type t     = bool
    type var_t = { name: string; mutable value: bool }

    let eval_exp exp = exp
    let var_to_s var = var.name 
  end

module BWashFSM = FSM(WashStates)(BoolExp) 
*)
(*********************************************************)



let suite = "Logic test suite" >::: [
             "test_construct" >:: test_construct;
             "test_expr_to_str" >:: test_expr_to_str;
             "test_reduce"      >:: test_reduce;
             "test_not"         >:: test_not;
             "test_and"         >:: test_and;
             "test_or"          >:: test_or;
             "test_xor"         >:: test_xor;
             "test_demorganize" >:: test_demorganize                               ] ;;

let _ =
  run_test_tt ~verbose:true suite ;;
