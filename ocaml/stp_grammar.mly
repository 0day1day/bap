
%token <string> VAR
%token <int64> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token ASSERT
%token INVALID
%token COMMA
%token END
%token EOF

%start main
%type <(string * int64) list> main

%%

main:
  /* The result comes before or after the assertions depending on version. */
  assertions result EOF { $1 }
| result assertions EOF { $2 }
  ;

assertions:
    /* empty */ { [] }
  | assertion SEMICOLON assertions { $1 :: $3 }
  ;

assertion:
  ASSERT LBRACKET VAR EQUAL VAL RBRACKET { ($3, $5) }
  ;

result:
  INVALID END { }
