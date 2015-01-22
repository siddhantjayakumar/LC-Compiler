open List

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


let rec eval_value env = function 
  | `Id i ->  eval_id env i 
  | `Int i  ->  i
  | `If (x,y,z)  -> eval_if env (x,y,z) 
  | `App (v1,v2) -> eval_app env (v1,v2)
  | _ -> -1

and eval_id env i = 
  match (List.Assoc.find env i) with
  | Some x -> x
  | None -> -1

and eval_if env (x,y,z) = if ((eval_value env x) <> 0) then (eval_value env y) else (eval_value env z)

and eval_func env (x,v) = -1
and eval_app env (v1, v2) =
  match v1 with  
  | `Func(x,valf) ->  let s = eval_value env v2 in 
                      eval_value ((x,s)::env) valf
  | _ -> -1

  







