%{
open Lang
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token FIRST
%token SECOND
%token FUN
%token FIX
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token COMMA
%token ARROW
%token DOT
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

%nonassoc IN ELSE ARROW
%left OR
%left AND
%nonassoc EQUALS
%left PLUS
%left TIMES

%start <Lang.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | e = app_expr { e }
  | e1 = expr; OR; e2 = expr { Or (e1, e2) }
  | e1 = expr; AND; e2 = expr { And (e1, e2) }
  | e1 = expr; EQUALS; e2 = expr { Equal (e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Plus (e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Mult (e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | FUN; x = ID; ARROW; e = expr { Fun (x, e) }
  | FIX; f = ID; DOT; x = ID; ARROW; e = expr { Fix (f, x, e) }
  ;

app_expr:
  | e = prefix_expr { e }
  | e1 = app_expr; e2 = prefix_expr { App (e1, e2) }
  ;

prefix_expr:
  | e = term_expr { e }
  | NOT; e = prefix_expr { Not e }
  | FIRST; e = prefix_expr { First e }
  | SECOND; e = prefix_expr { Second e }
  ;

term_expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN; e = expr; RPAREN { e }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair (e1, e2) }
  ;
