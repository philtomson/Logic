exception SizeMismatch ;;
type boolean = T | F | Vec of boolean array ;;
(*deriving(Show,Enum);;*)


(*type variable = Name of string | NameVal of string*boolean ;;*)
let to_bool v = match v with 
    T -> true
  | F -> false 
  | _ -> raise SizeMismatch;;

let rec b_to_s v = match v with
    T        -> "T"
  | F        -> "F" 
  | Vec(ary) -> (Array.fold_left (fun acc e -> acc^(b_to_s e)) "[" ary)^"]";;

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


