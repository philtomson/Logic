open Logic

type ('pred, 'ns) p_a_n = { pred: 'pred; 
                            actions: (bexp*boolean) list; 
                            ns: 'ns } deriving(Show);;

 module type STATES = 
    sig
      type t
        deriving (Show, Enum)

      val start_state : t
    end 

module FSM (States : STATES)  =
  struct 
    type t = States.t
    let start_state = States.start_state

    let enum_states = 
      let enum_types = Enum.enum_from<States.t> (Enum.to_enum<States.t> 0) in
      let enum_strings = List.map (fun et -> Show.show<States.t> et ) enum_types in
      String.concat ", " enum_strings 
   
    let state_to_s state = Show.show<States.t> state

    module ST_Table = Hashtbl.Make (
      struct
        type t = States.t
        let equal = (=)
        let hash = Hashtbl.hash
      end
    )
 
                      
    let create fsmtab  =   
      let stab = ST_Table.create 5 in
      List.iter (fun (cs, cond, ns, actions) ->  
        ST_Table.add stab cs { pred  = cond;
                               actions= actions; 
                               ns    = ns }) fsmtab; 
                               (stab, start_state)

    let find_all stab st = ST_Table.find_all stab st 


    let eval_fsm stab cs  = (*get next state*) 
      (*Printf.printf "  cs is: %s\n" (state_to_s cs);*)
      let targets = find_all stab cs in

      let rec find_next lst = match lst with
        []    -> None
      | x::xs -> if( to_bool (eval x.pred) ) then 
                 (
                   (*do actions*)
                   List.iter (fun (var, value) -> assign var value) x.actions;
                   Printf.printf "current state: %s  \tactions: %s \n" (state_to_s x.ns) (String.concat ", " (List.map (fun (var, value) ->
                                            var_to_s var) x.actions));
                   Some x.ns
                 )
                 else 
                   find_next xs      
                 in
      match (find_next targets) with
        None      -> cs (*stay in current state*)
      | Some s    -> s 

  end 
  
  (*Example usage*)
  open Logic
  
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
let reset_actions = 
  assign water_on      F;
  assign agitate       F;
  assign drain         F;
  assign start_timer   F;
  assign motor_on      F;;

module WashStates = 
  struct
   type t =  START | FILL_WSH | WASH | EMPTY | FILL_RNS | RINSE | SPIN | STOP
   deriving(Show, Enum)
     
   let start_state = START

  end ;;


module WashFSM = FSM(WashStates) ;;

open WashStates;;

              (* CS,     PREDICATE,  NS,       ACTIONs *)
let my_fsm = [(START,    Const(T),   FILL_WSH, [(water_on,   T)] );
              (FILL_WSH, full,       WASH,     [(water_on,   F);
                                                (agitate,    T);
                                                (start_timer,T)] );
              (WASH,     ten_minutes,EMPTY,    [(agitate,    F);
                                                (start_timer,F); 
                                                (drain,      T)] );
              (EMPTY,    empty,      FILL_RNS, [(drain,      F); 
                                                (water_on,   T)] );
              (FILL_RNS, full,       RINSE,    [(water_on,   F); 
                                                (agitate,    T)]);
              (RINSE,    ten_minutes,EMPTY,    [(agitate,    F);
                                                (drain,      T)] );
              (EMPTY,    empty,      SPIN,     [(motor_on,   T);
                                                (start_timer,T)]);
              (SPIN,     five_minutes,STOP,    [(water_on,   F);
                                                (drain,      F);
                                                (start_timer,F);
                                                (motor_on,   F)]);
              (STOP,     Const(T) ,  STOP,     [(motor_on,   F)]);
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

print_endline ( WashFSM.enum_states) ;; 
(****************************************
  
  
  
