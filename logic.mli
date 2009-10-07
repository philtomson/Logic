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
    val mk_and : bexp ->bexp ->bexp
    val mk_or :  bexp ->bexp ->bexp
    val mk_xor : bexp ->bexp ->bexp
    val mk_not : bexp ->bexp

