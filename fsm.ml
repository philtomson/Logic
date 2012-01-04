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

(*
 see WashFSM example in test_logic.ml
*)
