open Boolean

let port name width dir = 
  name ^ ": " ^ dir ^ 
  (if width = 1 then
    " std_logic "
  else
    (Printf.sprintf " std_logic_vector(%d downto 0)" width))
