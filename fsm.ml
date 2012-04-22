(*open Logic*)
open Vhdl


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
    type b
    type 'a var_t
    type 'a bexp
    val eval_exp   :  b bexp -> bool
    val assign     : 'a bexp -> 'a -> unit
    val get_inputs : 'a bexp -> 'a bexp list 
    val var_to_s   : b bexp -> string
    val var_name   : 'a var_t -> string
    val var_val    : 'a var_t -> 'a
    val get_var    : 'a bexp -> 'a var_t option
  end
 
(*
module type CODEGENERATOR = 
  sig
    type t 
    val to_code : t -> string
  end
*)

module FSM (States : STATES)(Exp : EXPRESSION)(*(CodeGen : CODEGENERATOR)*)  =
  struct 
    let start_state = States.start_state
    let eval_exp = Exp.eval_exp
    let assign      = Exp.assign
    let var_to_s    = Exp.var_to_s
    let get_var     = Exp.get_var

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
                         var_to_s var ) (x.actions ) ) );
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
        (Exp.get_inputs v.pred)::lst) stab []) in

      (*Collect inputs TODO: can be done in Logic?*)
      let inputs = 
        let rec aux inlst aclst = match inlst with
          []    -> aclst
        | e::es -> match (get_var e) with 
                     Some n -> aux es (n::aclst)
                   | None      -> aux es aclst  in
        (*was:| e::es -> match e with 
                     Var(n) -> aux es (n::aclst)
                   | _      -> aux es aclst  in *)
        aux pred_list [] in  

      (*uniqify list*)
      List.fold_left (fun res e ->
                        if List.mem e res then res
                        else (
                           e::res
                        )
                      ) [] inputs 


    let get_outputs stab =
      let action_list = List.flatten( ST_Table.fold ( fun _ v lst -> 
        (v.actions)::lst) stab []) in

      let outputs = 
        let rec aux outlst aclst = match outlst with
          []                -> aclst
        | (e,_)::es  -> ( match (get_var e) with 
                            Some n -> aux es (n::aclst) 
                          | None   -> aux es aclst 
                        )
        | _::es      -> aux es aclst in
        aux action_list [] in


      List.fold_left (fun res e ->
                        if List.mem e res then res
                        else (
                           e::res
                        )
                      ) []  outputs



    let intersection a b = 
      let lst1, lst2 = if (List.length a) > (List.length b) then (a,b)
                       else (b,a) in
      let rec aux a b accum = match a with
        []    -> accum
      | x::xs -> if (List.mem x b) then aux xs b (x::accum) 
                 else                   aux xs b accum     in
      aux lst1 lst2 [] 

                                
    let get_inouts stab = 
      let intrs = intersection  (get_inputs stab) (get_outputs stab) in
      intrs

      


    let to_code stab = 
      (* first analyze predicates to determine inputs*)
      let input_list  = get_inputs  stab in
      let output_list = get_outputs stab in
      let get_inouts  = get_inouts  stab in
      let out_str =  
        "Entity FSM is \n  
           port(\n" ^ 
         String.concat ";\n" (List.map (fun i ->
                   let name = Exp.var_name i in
                   let v    = Exp.var_val i in
                   "\t\t"^(port name (Boolean.width v) "in" )
                   )
                   input_list) ^ ");"  in
      out_str
            


  end 

(*
 see WashFSM example in test_logic.ml
*)
