
%token <string> VAR
%token <int64> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token MODEL
%token ASSERT
%token SDASHES
%token DASHES
%token INVALID
%token VALID
%token DEFAULT
%token COMMA
%token PERIOD
%token EOF

%start main
%type <(string * int64) list option> main

%%

main:
/* The result comes before or after the assertions depending on version. */
  goodresult MODEL assertions DASHES EOF { Some($3) }
| badresult EOF { None }
  ;

assertions:
  /* empty */ { [] }
  | assertion assertions { match $1 with | None -> $2 | Some(x) -> x::$2 }
  ;

assertion:
  /* Assign a bitvector to a variable */
  LBRACKET EQUAL VAR VAL RBRACKET { Some($3, $4) }
  /* Assign a variable to a variable */
  | LBRACKET EQUAL VAR VAR RBRACKET { None }
  /* Ignore "--- var ---" */
  | SDASHES VAR SDASHES { None }
  /* Ignore "default: val" */
  | DEFAULT VAL { None }
  /* Ignore memory information for now */
  | LBRACKET EQUAL LBRACKET VAR VAL RBRACKET VAL RBRACKET { None }
  ;

goodresult:
  INVALID { }
;

badresult:
  VALID { }
;
