(* The type of tokens. *)
type token = 
  | THEN
  | RIGHT
  | LEFT
  | INT of (int)
  | IF
  | ID of (string)
  | FUNC
  | EOF
  | ELSE
  | DOT

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (LC.value option)

