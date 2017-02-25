{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"    { comment lexbuf }

(* parens *)
| '('     { LPAREN } 
| ')'     { RPAREN }
| '{'     { LBRACE }
| '}'     { RBRACE }
| '['     { LBRACK }
| ']'     { RBRACK }

(* strings *)
| '"'     { DBL_QUOTE }
| '\''    { SNG_QUOTE }

(* separators *)
| ';'     { SEMI }
| ','     { COMMA }
| ".."    { DOTDOT }
| '.'     { DOT }

(* arithmetic operators *)
| '+'     { PLUS }
| '-'     { MINUS }
| '*'     { TIMES }
| '/'     { DIVIDE }
| '%'     { MOD }

(* assignment operators *)
| '='     { ASSIGN }
| "+="    { ADD_ASN }
| "-="    { SUB_ASN }
| "*="    { MUL_ASN }
| "/="    { DIV_ASN }
| "%="    { MOD_ASN }

(* logical operators *)
| "&&"    { LOG_AND }
| "||"    { LOG_OR }
| "!"     { LOG_NOT }

(* do we want to do this? 
(* bitwise operators *)
| '&'     { BIT_AND }
| '|'     { BIT_OR }
| '^'     { BIT_XOR }
| '~'     { BIT_NOT }
| "<<"    { LSHIFT }
| ">>"    { RSHIFT }
*)

(* comparison operators *)
| '<'     { LT }
| '>'     { GT }
| "=="    { EQ }
| "!="    { NEQ }
| "<="    { LEQ }
| ">="    { GEQ }

(* shuxxx *)
| '?'     { QUES }
| ':'     { COLON }
| "::"    { FILTER }
| '@'     { MAP }
| "->"    { FUNC }

(* control keywords *)
| "if"      { IF }
| "then"    { THEN }
| "elif"    { ELIF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "do"      { DO }

(* declarations *)
| "ns"      { NS }
| "gn"      { GN }
| "kn"      { KN }
| "struct"  { STRUCT }
| "let"     { LET }
| "var"     { VAR }

(* types *)
| "int"     { INT }
| "scalar"  { SCALAR }
| "string"  { STRING }
| "bool"    { BOOL }
| "vector"  { VECTOR }

| eof { EOF }
