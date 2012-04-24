exception SizeMismatch
val log_base_int : int -> int -> int
type boolean = T | F | Vec of boolean array
val int_to_barray : int -> int -> boolean array
val barray_to_int : boolean array -> int
val int_to_b : int -> int -> boolean
val b_to_int : boolean -> int
val width : boolean -> int
val rel : (int -> int -> bool) -> boolean -> boolean -> bool
val ( >? ) : boolean -> boolean -> bool
val ( <? ) : boolean -> boolean -> bool
val ( >=? ) : boolean -> boolean -> bool
val ( <=? ) : boolean -> boolean -> bool
val increment : boolean -> boolean
val decrement : boolean -> boolean
val to_bool : boolean -> bool
val b_to_s : boolean -> string
val print_bool_lst : boolean list -> 'a list
val shiftr : boolean -> int -> boolean
val shiftl : boolean -> int -> boolean
val and_ : boolean -> boolean -> boolean
val or_ : boolean -> boolean -> boolean
val n : boolean -> boolean
val xor : boolean -> boolean -> boolean

