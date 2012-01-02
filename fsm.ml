open Logic;;
(*
type 'a state = State of 'a ;;
type ('cs, 'cond, 'action, 'ns) st_entry = ST_Entry of ('cs * 'cond * 'action * 'ns);;
type state_list = st_entry list ;;
type ('cs, 'cond) st_cond = State_Cond of ('cs * 'cond) ;;

type ('action, 'ns) st_action_ns = Action_NS of ('action * 'ns);;
*)

type ('pred, 'ns) p_a_n = { pred: 'pred; 
                            action: string; (*for now*)
                            ns: 'ns };;

 module type STATES = 
    sig
      type t
      val start_state : t
    end ;;

module FSM (States : STATES)  =
(*module FSM = functor (States : STATES) ->*)
(*module FSM (States : STATES) (ST : Hashtbl.HashedType) =*)
(*module FSM (States : STATES) (ST : Hashtbl.S) =*)
  struct 
    type t = States.t
    let start_state = States.start_state
  
    module ST_Table = Hashtbl.Make (
      struct
        type t = States.t
        let equal = (=)
        let hash = Hashtbl.hash
      end
    )

                      
    let create fsmtab  =   
      let stab = ST_Table.create 5 in
      List.iter (fun (cs, cond, action, ns) ->  
        ST_Table.add stab cs { pred  = cond;
                               action= action; 
                               ns    = ns }) fsmtab; 
                               (stab, start_state)

    let find_all stab st = ST_Table.find_all stab st 

    let next stab cs = (*get next state*) 
      let targets = find_all stab cs in

      let rec find_next lst = match lst with
        []    -> None
      | x::xs -> if( to_bool (eval x.pred) ) then Some x.ns
                 (*TODO: action is side effect; put it here?*)
                 (* ex: assign action T *)

                 else find_next xs      in
      match (find_next targets) with
        None   -> cs (*stay in current state*)
      | Some s -> s 

      (* TODO: ^^need to return not just the current state but also
       * the action *)


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
