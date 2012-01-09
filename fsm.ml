open Logic


type ('pred, 'ns, 'exp, 'btype) p_a_n = { pred: 'pred; 
                            actions: ('exp*'btype) list; 
                            ns: 'ns } deriving(Show);;

 module type STATES = 
    sig
      type t
        deriving (Show, Enum)

      val start_state : t
      
    end 

module type EXPRESSION = 
  sig
    type t
    type var_t
    val eval_exp : t -> bool

    val var_to_s : t -> string
  end

(*
module type CODEGENERATOR = 
  sig
    type t 
    val to_string : t -> string
  end
*)

module FSM (States : STATES)(Exp : EXPRESSION)(*(CodeGen : CODEGENERATOR)*)  =
  struct 
    type t = States.t
    let start_state = States.start_state

    type vt      = Exp.var_t
    let eval_exp = Exp.eval_exp

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
      | x::xs -> if( eval_exp x.pred ) then 
                 (
                   (*do actions*)
                   List.iter (fun (var, value) -> assign var value) x.actions;
                   Printf.printf "current state: %s  \tactions: %s \n" (state_to_s x.ns) (String.concat ", " 
                   (List.map (fun (var, value) ->
                      ( var_to_s var) ) x.actions ) );
                   Some x.ns
                 )
                 else 
                   find_next xs      
                 in
      match (find_next targets) with
        None      -> Printf.printf "NO CHANGE: current state: %s  \n"
                       (state_to_s cs) ;
                     cs (*stay in current state*)
      | Some s    -> s 

    let get_inputs stab = 

      let pred_list = List.flatten(ST_Table.fold ( fun _ v lst -> 
        (get_inputs v.pred)::lst) stab []) in

      let inputs = 
        let rec aux inlst aclst = match inlst with
          []    -> aclst
        | e::es -> match e with 
                     Var(n) -> aux es (n::aclst)
                   | _      -> aux es aclst  in
        aux pred_list [] in  

      (*uniqify list*)
      List.fold_left (fun res e ->
                        if List.mem e res then res
                        else (
                           Printf.printf "input: %s\n" e.name;
                           e::res
                        )
                      ) [] inputs 


    let get_outputs stab =
      let action_list = List.flatten( ST_Table.fold ( fun _ v lst -> 
        (v.actions)::lst) stab []) in

      let outputs = 
        let rec aux outlst aclst = match outlst with
          []                -> aclst
        | (Var(out),_)::es  -> aux es (out::aclst) 
        | _::es             -> aux es aclst in
        aux action_list [] in


        List.fold_left (fun res e ->
                          if List.mem e res then res
                          else (
                             Printf.printf "output: %s\n" e.name;
                             e::res
                          )
                        ) []  outputs

    (*let both stab  *)

      

(*
    let to_code stab = 
      (* first analyze predicates to determine inputs*)
      let input_list = 
      
      (* 2nd analyze actions to determine ouputs - easier*)
      (* 3rd find the intersection between the two lists to 
       * determine inouts *)
*)

  end 

(*
 see WashFSM example in test_logic.ml
*)
