%token <int> INT
%token <string> ID
%token IF
%token ELSE
%token LEFT
%token RIGHT
%token THEN
%token EOF
%token FUNC
%token DOT
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
  | IF; cond = value; THEN; t = value; ELSE; e = value;
    {`If(cond,t,e)}
  |FUNC; x = ID; DOT; v = value;
   { `Func(x,v) }
  |LEFT; v = value; RIGHT; v2 = value;
   { `App(v,v2)}
;

