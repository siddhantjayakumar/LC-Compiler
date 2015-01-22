type value = [
  | `Id of string
  | `Int of int
  | `If of (value * value * value)
  | `Func of (string * value) 
  | `App of (value * value)
 ]

(* part 1 *)
open Core.Std
let rec output_value outc = function
  | `Id i -> printf "id %s" i
  | `Int i  -> printf "int %d" i
  | `If (x,y,z)   -> print_if outc (x,y,z) 
  | `Func (x,v) -> print_func outc (x,v) 
  | `App (v1,v2) -> print_app outc (v1,v2)
  |_ -> printf "Error"

and print_if outc (x,y,z) = 
  output_string outc "An if with cond "; output_value outc x; output_string outc " and then "; output_value outc y; output_string outc " else "; output_value outc z

and print_func outc (x,v) =  output_string outc "A function with var: "; output_string outc x; output_string outc " and value "; output_value outc v

and print_app outc (v1,v2) =  output_string outc "An app with fun: "; output_value outc v1; output_string outc " and subs "; output_value outc v2
