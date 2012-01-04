open Logic;;
(*
type 'a state = State of 'a ;;
type ('cs, 'cond, 'action, 'ns) st_entry = ST_Entry of ('cs * 'cond * 'action * 'ns);;
type state_list = st_entry list ;;
type ('cs, 'cond) st_cond = State_Cond of ('cs * 'cond) ;;

type ('action, 'ns) st_action_ns = Action_NS of ('action * 'ns);;
*)

type ('pred, 'ns) p_a_n = { pred: 'pred; 
                            actions: (bexp*boolean) list; (*for now*)
                            ns: 'ns } deriving(Show);;

 module type STATES = 
    sig
      type t
        deriving (Show, Enum)

      val start_state : t
    end ;;

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

  end ;;    

(*
module WashStates = 
  struct
   type t =  FILL_WSH | WASH | EMPTY | FILL_RNS | RINSE | SPIN | STOP 
   let start_state = FILL_WSH 
  end ;;

module WashFSM = FSM(WashStates) ;;

let my_fsm = [(WashStates.FILL_WSH, "full",  "water_on",  WashStates.WASH);
              (WashStates.WASH, "10Minutes", "agitate",   WashStates.EMPTY);
              (WashStates.EMPTY,"empty",     "drain",     WashStates.FILL_RNS);
              (WashStates.FILL_RNS,"full",   "water_on",  WashStates.RINSE);
              (WashStates.RINSE,"10Minutes", "agitate",   WashStates.EMPTY);
              (WashStates.EMPTY,"empty",     "drain",     WashStates.SPIN);
              (WashStates.SPIN, "5Minutes",  "motor_on",  WashStates.STOP);
              (WashStates.STOP, "*",         "motor_off", WashStates.STOP);
             ];; 

let st_table = WashFSM.create my_fsm;;
WashFSM.find_all WashStates.FILL_WSH st_table;;
*)
