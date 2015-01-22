type value = [
  | `Int of int
  | `If of (value * value * value)
  | `Func of (string * value) 
  | `App of (value * value)
 ]

(* part 1 *)
open Core.Std
let rec output_value outc = function
  | `Int i  -> printf "int %d" i
  | `If (x,y,z)   -> print_if outc (x,y,z) 
                    
  |_ -> printf "Error"

and print_if outc (x,y,z) = 
  output_string outc "An if with cond "; output_value outc x; output_string outc " and then "; output_value outc y; output_string outc " else "; output_value outc z


