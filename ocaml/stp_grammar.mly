
%token <string> VAR
%token <int64> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token ASSERT
%token INVALID
%token VALID
%token SAT
%token DASH
%token MODEL
%token COMMA
%token END
%token EOF

%start main
%type <(string * int64) list option> main

%%

  main:
/* The result comes before or after the assertions depending on version. */
  assertions goodresult EOF { Some($1) }
| goodresult assertions EOF { Some($2) }
| goodresult assertions DASH DASH DASH DASH EOF { Some($2) }
| badresult EOF { None }
  ;
  
  assertions:
  /* empty */ { [] }
| assertion SEMICOLON assertions { $1 :: $3 }
| assertion assertions { $1 :: $2 }
  ;
  
  assertion:
  ASSERT LBRACKET VAR EQUAL VAL RBRACKET { ($3, $5) } /* STP style */
| LBRACKET EQUAL VAR VAL RBRACKET { ($3, $4) }         /* YICES style */
  ;
  
  goodresult:
  INVALID END { }
| SAT MODEL { }
  ;
  
badresult:
  VALID END { }
;
