type token =
  | FUN
  | LET
  | REC
  | IN
  | APP
  | VAR of (string)
  | IMP
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EQ
  | LEQ
  | LT
  | GEQ
  | GT
  | IF
  | THEN
  | ELSE
  | NUM of (int)
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
