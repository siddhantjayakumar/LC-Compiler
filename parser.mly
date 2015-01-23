%token <int> INT
%token <string> ID
%token KWIF
%token KWELSE
%token LEFTPAREN
%token RIGHTPAREN
%token KWTHEN
%token EOF
%token KWFUN
%token ARROW
%start <LC.value option> prog
%%

prog:
  | EOF       { None }
  | v = value { Some v }
  ;

value:
  | d = ID
   {`Id d}
  | i = INT;
    {`Int i}
  | KWIF; cond = value; KWTHEN; t = value; KWELSE; e = value;
    {`If(cond,t,e)}
  |KWFUN; x = ID; ARROW; v = value;
   { `Func(x,v) }
  |LEFTPAREN; v = value; RIGHTPAREN; v2 = value;
   { `App(v,v2)}
;

