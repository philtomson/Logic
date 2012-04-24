exception VarNotDefined
exception NotAVariable
type 'a var_t = { name : string; mutable value : 'a; }
(*
module Show_var_t :
  functor (M_a : Deriving_Show.Show) ->
    sig
      type a = M_a.a var_t
      val format : Format.formatter -> a -> unit
      val format_list : Format.formatter -> a list -> unit
      val show : a -> string
      val show_list : a list -> string
    end
*)
type 'a bexp =
    Const of 'a
  | Var of 'a var_t
  | Bop of bop * 'a bexp * 'a bexp
  | Not of 'a bexp
and bop = And | Or | Xor
val var_name : 'a var_t -> string
val var_val : 'a var_t -> 'a
val var_to_s : Boolean.boolean bexp -> string
val bop_to_func :
  bop -> Boolean.boolean -> Boolean.boolean -> Boolean.boolean
val ( *: ) : Boolean.boolean -> Boolean.boolean -> Boolean.boolean
val ( +: ) : Boolean.boolean -> Boolean.boolean -> Boolean.boolean
val ( ^: ) : Boolean.boolean -> Boolean.boolean -> Boolean.boolean
val ( !: ) : Boolean.boolean -> Boolean.boolean
val op_to_str : bop -> string
val expr_to_str : Boolean.boolean bexp -> string
val reduce : Boolean.boolean bexp -> Boolean.boolean bexp
val demorganize : 'a bexp -> 'a bexp
val assign : 'a bexp -> 'a -> unit
val eval : Boolean.boolean bexp -> Boolean.boolean
val get_inputs : 'a bexp -> 'a bexp list
val get_var : 'a bexp -> 'a var_t option
val literal_count : 'a bexp -> int
val op_count : 'a bexp -> int
val mk_and : 'a bexp -> 'a bexp -> 'a bexp
val mk_or : 'a bexp -> 'a bexp -> 'a bexp
val mk_xor : 'a bexp -> 'a bexp -> 'a bexp
val mk_not : 'a bexp -> 'a bexp
val mk_var : 'a var_t -> 'a bexp
val mk_const : 'a -> 'a bexp
val bin_funcs : ('a bexp -> 'a bexp -> 'a bexp) list
val choose_rand_bin_func : 'a -> 'b bexp -> 'b bexp -> 'b bexp
val do_with_prob : ('a -> 'b) -> 'a -> 'b -> float -> 'b
val do_with_prob' : 'a bexp -> 'a bexp list -> float -> 'a bexp
val mutate_with_prob' : 'a bexp -> 'a bexp list -> float -> 'a bexp
val do_bin_with_prob : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c -> float -> 'c
val num_ops : int
val bop_to_s : bop -> string
type uop = NEG | P_I
val uop_to_s : uop -> string
type 'a unary_op = uop * ('a bexp -> 'a bexp)
type 'a binary_op = bop * ('a bexp -> 'a bexp -> 'a bexp)
type 'a any_arity_func =
    UnaryFunc of 'a unary_op
  | BinaryFunc of 'a binary_op
val unary_operations : 'a any_arity_func list
val binary_operations : 'a any_arity_func list
val operations : Boolean.boolean any_arity_func list
val choose_rand_op : 'a -> Boolean.boolean any_arity_func
val choose_rand_bin_op : 'a -> 'b any_arity_func
val choose_rand_unary_op : 'a -> 'b any_arity_func
val rand_bin_op : 'a bexp * 'a bexp -> 'a bexp
val grow_rand_tree : int -> Boolean.boolean bexp list -> Boolean.boolean bexp
val list_by_pairs : 'a list -> ('a * 'a) list
val rand_not : 'a bexp -> 'a bexp
val make_tree_from_list : Boolean.boolean bexp list -> Boolean.boolean bexp
val get_random_input : 'a list -> 'a
val cross : 'a bexp -> 'a bexp -> 'a bexp * 'a bexp
val incr_bin : Boolean.boolean list -> Boolean.boolean list
val count_bin :
  Boolean.boolean list ->
  (Boolean.boolean list -> 'a) -> Boolean.boolean list

