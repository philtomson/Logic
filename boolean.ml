exception SizeMismatch ;;
type boolean = T | F | Vec of boolean array ;;
(*     deriving(Show,Enum);; *)

(*
let rec int_to_bool n acc = if n=0 then
                              acc
                            else 
                              int_to_bool (n lsr 1) ((n mod 2)::acc) ;;
*)

(*convert an integer to a boolean array*)
(*NOTE: we don't handled signed integers at this point*)
let int_to_barray num bits = 
  let rec aux n idx acc =
  if (idx=bits) then
    acc
  else 
  (
    (Array.set acc idx (if (n mod 2) = 0 then F
                        else T ));
    aux (n lsr 1) (idx+1) acc  
  ) in
  aux num 0 (Array.make bits F);;

(*convert a boolean array to an integer*)
let barray_to_int ba = 
  let rec aux idx num = 
    if (idx=(Array.length ba)) then
      num
    else
      if ba.(idx) = T then
        aux (idx+1) 
            num+(int_of_float(2.0**(float_of_int idx)))
      else
        aux (idx+1) num
    in
  aux 0 0;;


let rel op x y = match x,y with
    (Vec(a), Vec(b)) -> (op (barray_to_int a) (barray_to_int b))
  | (Vec(a), T ) -> (op (barray_to_int a)  1)
  | (Vec(a), F ) -> (op (barray_to_int a)  0)
  | (T, Vec(b) ) -> (op 1  (barray_to_int b))
  | (F, Vec(b) ) -> (op 0  (barray_to_int b))
  | (T, F)       -> true
  | (F, T)       -> false
  | _            -> false ;;

let ( >? ) x y = rel ( > ) x y ;;
let ( <? ) x y = rel ( < ) x y ;;
let ( >=? ) x y = rel ( >= ) x y ;;
let ( <=? ) x y = rel ( <= ) x y ;;



(*type variable = Name of string | NameVal of string*boolean ;;*)
let to_bool v = match v with 
    T -> true
  | F -> false 
  | _ -> raise SizeMismatch;;

let rec b_to_s v = match v with
    T        -> "T"
  | F        -> "F" 
  | Vec(ary) -> "["^(Array.fold_left (fun acc e -> (b_to_s e)^acc) "" ary)^"]";;

let rec print_bool_lst lst = match lst with 
    [] -> (Printf.printf "\n"); []
  | x::xs -> (Printf.printf "%s " (b_to_s x)); print_bool_lst xs ;;

let rec and_ x y = match x,y with
    (T,T)             -> T
  | (F,_) | (_,F)     -> F
  | (Vec(ary1),Vec(ary2)) -> 
      if (Array.length ary1) <> (Array.length ary2) then
        raise SizeMismatch
      else
        Vec(Array.mapi (fun idx e -> (and_ e (ary2.(idx))))  ary1)
  | (Vec _, _ ) | (_, Vec _) -> raise SizeMismatch;;

let rec or_ x y = match x,y with
    (_,T) | (T,_) -> T
  | (F,F)         -> F 
  | (Vec(ary1),Vec(ary2)) ->
      if (Array.length ary1) <> (Array.length ary2) then
        raise SizeMismatch
      else
        Vec(Array.mapi (fun idx e -> (or_ e (ary2.(idx))))  ary1)
  | (Vec _, _ ) | (_, Vec _) -> raise SizeMismatch;;


let rec n x = match x with
    T -> F
  | F -> T 
  | Vec ary -> Vec(Array.map (fun e -> n e ) ary) ;;

let rec xor x y = match x,y with
    (T, F) | (F, T) -> T
  | (Vec(ary1),Vec(ary2)) ->
      if (Array.length ary1) <> (Array.length ary2) then
        raise SizeMismatch
      else
        Vec(Array.mapi (fun idx e -> (xor e (ary2.(idx))))  ary1)
  | (Vec _, _ ) | (_, Vec _) -> raise SizeMismatch
  | _ -> F;;


