(* File generated from z3.idl *)

type config
and context
and sort
and func_decl
and ast
and app
and pattern
and symbol
and parameter
and model
and literals
and constructor
and constructor_list
and enum_1 =
  | L_FALSE
  | L_UNDEF
  | L_TRUE
and lbool = enum_1
and enum_2 =
  | INT_SYMBOL
  | STRING_SYMBOL
and symbol_kind = enum_2
and enum_3 =
  | PARAMETER_INT
  | PARAMETER_DOUBLE
  | PARAMETER_RATIONAL
  | PARAMETER_SYMBOL
  | PARAMETER_SORT
  | PARAMETER_AST
  | PARAMETER_FUNC_DECL
and parameter_kind = enum_3
and enum_4 =
  | UNINTERPRETED_SORT
  | BOOL_SORT
  | INT_SORT
  | REAL_SORT
  | BV_SORT
  | ARRAY_SORT
  | DATATYPE_SORT
  | UNKNOWN_SORT
and sort_kind = enum_4
and enum_5 =
  | NUMERAL_AST
  | APP_AST
  | VAR_AST
  | QUANTIFIER_AST
  | UNKNOWN_AST
and ast_kind = enum_5
and enum_6 =
  | OP_TRUE
  | OP_FALSE
  | OP_EQ
  | OP_DISTINCT
  | OP_ITE
  | OP_AND
  | OP_OR
  | OP_IFF
  | OP_XOR
  | OP_NOT
  | OP_IMPLIES
  | OP_OEQ
  | OP_ANUM
  | OP_LE
  | OP_GE
  | OP_LT
  | OP_GT
  | OP_ADD
  | OP_SUB
  | OP_UMINUS
  | OP_MUL
  | OP_DIV
  | OP_IDIV
  | OP_REM
  | OP_MOD
  | OP_TO_REAL
  | OP_STORE
  | OP_SELECT
  | OP_CONST_ARRAY
  | OP_ARRAY_DEFAULT
  | OP_STORE_ITE
  | OP_SET_UNION
  | OP_SET_INTERSECT
  | OP_SET_DIFFERENCE
  | OP_SET_COMPLEMENT
  | OP_SET_SUBSET
  | OP_BNUM
  | OP_BIT1
  | OP_BIT0
  | OP_BNEG
  | OP_BADD
  | OP_BSUB
  | OP_BMUL
  | OP_BSDIV
  | OP_BUDIV
  | OP_BSREM
  | OP_BUREM
  | OP_BSMOD
  | OP_BSDIV0
  | OP_BUDIV0
  | OP_BSREM0
  | OP_BUREM0
  | OP_BSMOD0
  | OP_ULEQ
  | OP_SLEQ
  | OP_UGEQ
  | OP_SGEQ
  | OP_ULT
  | OP_SLT
  | OP_UGT
  | OP_SGT
  | OP_BAND
  | OP_BOR
  | OP_BNOT
  | OP_BXOR
  | OP_BNAND
  | OP_BNOR
  | OP_BXNOR
  | OP_CONCAT
  | OP_SIGN_EXT
  | OP_ZERO_EXT
  | OP_EXTRACT
  | OP_REPEAT
  | OP_BREDOR
  | OP_BREDAND
  | OP_BCOMP
  | OP_BSHL
  | OP_BLSHR
  | OP_BASHR
  | OP_ROTATE_LEFT
  | OP_ROTATE_RIGHT
  | OP_INT2BV
  | OP_BV2INT
  | OP_PR_UNDEF
  | OP_PR_TRUE
  | OP_PR_ASSERTED
  | OP_PR_GOAL
  | OP_PR_MODUS_PONENS
  | OP_PR_REFLEXIVITY
  | OP_PR_SYMMETRY
  | OP_PR_TRANSITIVITY
  | OP_PR_TRANSITIVITY_STAR
  | OP_PR_MONOTONICITY
  | OP_PR_QUANT_INTRO
  | OP_PR_DISTRIBUTIVITY
  | OP_PR_AND_ELIM
  | OP_PR_NOT_OR_ELIM
  | OP_PR_REWRITE
  | OP_PR_REWRITE_STAR
  | OP_PR_PULL_QUANT
  | OP_PR_PULL_QUANT_STAR
  | OP_PR_PUSH_QUANT
  | OP_PR_ELIM_UNUSED_VARS
  | OP_PR_DER
  | OP_PR_QUANT_INST
  | OP_PR_HYPOTHESIS
  | OP_PR_LEMMA
  | OP_PR_UNIT_RESOLUTION
  | OP_PR_IFF_TRUE
  | OP_PR_IFF_FALSE
  | OP_PR_COMMUTATIVITY
  | OP_PR_DEF_AXIOM
  | OP_PR_DEF_INTRO
  | OP_PR_APPLY_DEF
  | OP_PR_IFF_OEQ
  | OP_PR_NNF_POS
  | OP_PR_NNF_NEG
  | OP_PR_NNF_STAR
  | OP_PR_CNF_STAR
  | OP_PR_SKOLEMIZE
  | OP_PR_MODUS_PONENS_OEQ
  | OP_PR_TH_LEMMA
  | OP_UNINTERPRETED
and decl_kind = enum_6
and enum_7 =
  | NO_FAILURE
  | UNKNOWN
  | TIMEOUT
  | MEMOUT_WATERMARK
  | CANCELED
  | NUM_CONFLICTS
  | THEORY
  | QUANTIFIERS
and search_failure = enum_7
and enum_8 =
  | PRINT_SMTLIB_FULL
  | PRINT_LOW_LEVEL
and ast_print_mode = enum_8

external mk_config : unit -> config
	= "camlidl_z3_Z3_mk_config"

external del_config : config -> unit
	= "camlidl_z3_Z3_del_config"

external set_param_value : config -> string -> string -> unit
	= "camlidl_z3_Z3_set_param_value"

external mk_context : config -> context
	= "camlidl_z3_Z3_mk_context"

external del_context : context -> unit
	= "camlidl_z3_Z3_del_context"

external trace_to_file : context -> string -> bool
	= "camlidl_z3_Z3_trace_to_file"

external trace_to_stderr : context -> unit
	= "camlidl_z3_Z3_trace_to_stderr"

external trace_to_stdout : context -> unit
	= "camlidl_z3_Z3_trace_to_stdout"

external trace_off : context -> unit
	= "camlidl_z3_Z3_trace_off"

external mk_int_symbol : context -> int -> symbol
	= "camlidl_z3_Z3_mk_int_symbol"

external mk_string_symbol : context -> string -> symbol
	= "camlidl_z3_Z3_mk_string_symbol"

external is_eq_sort : context -> sort -> sort -> bool
	= "camlidl_z3_Z3_is_eq_sort"

external mk_uninterpreted_sort : context -> symbol -> sort
	= "camlidl_z3_Z3_mk_uninterpreted_sort"

external mk_bool_sort : context -> sort
	= "camlidl_z3_Z3_mk_bool_sort"

external mk_int_sort : context -> sort
	= "camlidl_z3_Z3_mk_int_sort"

external mk_real_sort : context -> sort
	= "camlidl_z3_Z3_mk_real_sort"

external mk_bv_sort : context -> int -> sort
	= "camlidl_z3_Z3_mk_bv_sort"

external mk_array_sort : context -> sort -> sort -> sort
	= "camlidl_z3_Z3_mk_array_sort"

external mk_tuple_sort : context -> symbol -> symbol array -> sort array -> sort * func_decl * func_decl array
	= "camlidl_z3_Z3_mk_tuple_sort"

external mk_enumeration_sort : context -> symbol -> symbol array -> sort * func_decl array * func_decl array
	= "camlidl_z3_Z3_mk_enumeration_sort"

external mk_list_sort : context -> symbol -> sort -> sort * func_decl * func_decl * func_decl * func_decl * func_decl * func_decl
	= "camlidl_z3_Z3_mk_list_sort"

external mk_constructor : context -> symbol -> symbol -> symbol array -> sort array -> int array -> constructor
	= "camlidl_z3_Z3_mk_constructor_bytecode" "camlidl_z3_Z3_mk_constructor"

external query_constructor : context -> constructor -> int -> func_decl * func_decl * func_decl array
	= "camlidl_z3_Z3_query_constructor"

external del_constructor : context -> constructor -> unit
	= "camlidl_z3_Z3_del_constructor"

external mk_datatype : context -> symbol -> constructor array -> sort * constructor array
	= "camlidl_z3_Z3_mk_datatype"

external mk_constructor_list : context -> constructor array -> constructor_list
	= "camlidl_z3_Z3_mk_constructor_list"

external del_constructor_list : context -> constructor_list -> unit
	= "camlidl_z3_Z3_del_constructor_list"

external mk_datatypes : context -> symbol array -> constructor_list array -> sort array * constructor_list array
	= "camlidl_z3_Z3_mk_datatypes"

external mk_injective_function : context -> symbol -> sort array -> sort -> func_decl
	= "camlidl_z3_Z3_mk_injective_function"

external is_eq_ast : context -> ast -> ast -> bool
	= "camlidl_z3_Z3_is_eq_ast"

external mk_func_decl : context -> symbol -> sort array -> sort -> func_decl
	= "camlidl_z3_Z3_mk_func_decl"

external mk_app : context -> func_decl -> ast array -> ast
	= "camlidl_z3_Z3_mk_app"

external mk_const : context -> symbol -> sort -> ast
	= "camlidl_z3_Z3_mk_const"

external mk_label : context -> symbol -> bool -> ast -> ast
	= "camlidl_z3_Z3_mk_label"

external mk_fresh_func_decl : context -> string -> sort array -> sort -> func_decl
	= "camlidl_z3_Z3_mk_fresh_func_decl"

external mk_fresh_const : context -> string -> sort -> ast
	= "camlidl_z3_Z3_mk_fresh_const"

external mk_true : context -> ast
	= "camlidl_z3_Z3_mk_true"

external mk_false : context -> ast
	= "camlidl_z3_Z3_mk_false"

external mk_eq : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_eq"

external mk_distinct : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_distinct"

external mk_not : context -> ast -> ast
	= "camlidl_z3_Z3_mk_not"

external mk_ite : context -> ast -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_ite"

external mk_iff : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_iff"

external mk_implies : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_implies"

external mk_xor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_xor"

external mk_and : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_and"

external mk_or : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_or"

external mk_add : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_add"

external mk_mul : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_mul"

external mk_sub : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_sub"

external mk_unary_minus : context -> ast -> ast
	= "camlidl_z3_Z3_mk_unary_minus"

external mk_div : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_div"

external mk_mod : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_mod"

external mk_rem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_rem"

external mk_lt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_lt"

external mk_le : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_le"

external mk_gt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_gt"

external mk_ge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_ge"

external mk_int2real : context -> ast -> ast
	= "camlidl_z3_Z3_mk_int2real"

external mk_bvnot : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnot"

external mk_bvand : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvand"

external mk_bvor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvor"

external mk_bvxor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvxor"

external mk_bvnand : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnand"

external mk_bvnor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnor"

external mk_bvxnor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvxnor"

external mk_bvneg : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvneg"

external mk_bvadd : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvadd"

external mk_bvsub : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsub"

external mk_bvmul : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvmul"

external mk_bvudiv : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvudiv"

external mk_bvsdiv : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsdiv"

external mk_bvurem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvurem"

external mk_bvsrem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsrem"

external mk_bvsmod : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsmod"

external mk_bvult : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvult"

external mk_bvslt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvslt"

external mk_bvule : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvule"

external mk_bvsle : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsle"

external mk_bvuge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvuge"

external mk_bvsge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsge"

external mk_bvugt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvugt"

external mk_bvsgt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsgt"

external mk_concat : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_concat"

external mk_extract : context -> int -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_extract"

external mk_sign_ext : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_sign_ext"

external mk_zero_ext : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_zero_ext"

external mk_bvshl : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvshl"

external mk_bvlshr : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvlshr"

external mk_bvashr : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvashr"

external mk_rotate_left : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_rotate_left"

external mk_rotate_right : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_rotate_right"

external mk_int2bv : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_int2bv"

external mk_bv2int : context -> ast -> bool -> ast
	= "camlidl_z3_Z3_mk_bv2int"

external mk_bvadd_no_overflow : context -> ast -> ast -> bool -> ast
	= "camlidl_z3_Z3_mk_bvadd_no_overflow"

external mk_bvadd_no_underflow : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvadd_no_underflow"

external mk_bvsub_no_overflow : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsub_no_overflow"

external mk_bvsub_no_underflow : context -> ast -> ast -> bool -> ast
	= "camlidl_z3_Z3_mk_bvsub_no_underflow"

external mk_bvsdiv_no_overflow : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsdiv_no_overflow"

external mk_bvneg_no_overflow : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvneg_no_overflow"

external mk_bvmul_no_overflow : context -> ast -> ast -> bool -> ast
	= "camlidl_z3_Z3_mk_bvmul_no_overflow"

external mk_bvmul_no_underflow : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvmul_no_underflow"

external mk_select : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_select"

external mk_store : context -> ast -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_store"

external mk_const_array : context -> sort -> ast -> ast
	= "camlidl_z3_Z3_mk_const_array"

external mk_array_default : context -> ast -> ast
	= "camlidl_z3_Z3_mk_array_default"

external mk_numeral : context -> string -> sort -> ast
	= "camlidl_z3_Z3_mk_numeral"

external mk_int : context -> int -> sort -> ast
	= "camlidl_z3_Z3_mk_int"

external mk_unsigned_int : context -> int -> sort -> ast
	= "camlidl_z3_Z3_mk_unsigned_int"

external mk_pattern : context -> ast array -> pattern
	= "camlidl_z3_Z3_mk_pattern"

external mk_bound : context -> int -> sort -> ast
	= "camlidl_z3_Z3_mk_bound"

external mk_forall : context -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	= "camlidl_z3_Z3_mk_forall_bytecode" "camlidl_z3_Z3_mk_forall"

external mk_exists : context -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	= "camlidl_z3_Z3_mk_exists_bytecode" "camlidl_z3_Z3_mk_exists"

external mk_quantifier : context -> bool -> int -> int -> pattern -> int -> sort -> symbol -> ast -> ast
	= "camlidl_z3_Z3_mk_quantifier_bytecode" "camlidl_z3_Z3_mk_quantifier"

external get_symbol_kind : context -> symbol -> symbol_kind
	= "camlidl_z3_Z3_get_symbol_kind"

external get_symbol_int : context -> symbol -> int
	= "camlidl_z3_Z3_get_symbol_int"

external get_symbol_string : context -> symbol -> string
	= "camlidl_z3_Z3_get_symbol_string"

external get_ast_kind : context -> ast -> ast_kind
	= "camlidl_z3_Z3_get_ast_kind"

external get_numeral_string : context -> ast -> string
	= "camlidl_z3_Z3_get_numeral_string"

external get_numeral_small : context -> ast -> bool * int64 * int64
	= "camlidl_z3_Z3_get_numeral_small"

external get_numeral_int : context -> ast -> bool * int
	= "camlidl_z3_Z3_get_numeral_int"

external get_numeral_uint : context -> ast -> bool * int
	= "camlidl_z3_Z3_get_numeral_uint"

external get_bool_value : context -> ast -> lbool
	= "camlidl_z3_Z3_get_bool_value"

external get_app_decl : context -> app -> func_decl
	= "camlidl_z3_Z3_get_app_decl"

external get_app_num_args : context -> app -> int
	= "camlidl_z3_Z3_get_app_num_args"

external get_app_arg : context -> app -> int -> ast
	= "camlidl_z3_Z3_get_app_arg"

external get_index_value : context -> ast -> int
	= "camlidl_z3_Z3_get_index_value"

external is_quantifier_forall : context -> ast -> bool
	= "camlidl_z3_Z3_is_quantifier_forall"

external get_quantifier_weight : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_weight"

external get_quantifier_num_patterns : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_num_patterns"

external get_quantifier_pattern_ast : context -> ast -> int -> pattern
	= "camlidl_z3_Z3_get_quantifier_pattern_ast"

external get_quantifier_bound_name : context -> ast -> int -> symbol
	= "camlidl_z3_Z3_get_quantifier_bound_name"

external get_quantifier_bound_sort : context -> ast -> int -> sort
	= "camlidl_z3_Z3_get_quantifier_bound_sort"

external get_quantifier_body : context -> ast -> ast
	= "camlidl_z3_Z3_get_quantifier_body"

external get_quantifier_num_bound : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_num_bound"

external get_decl_name : context -> func_decl -> symbol
	= "camlidl_z3_Z3_get_decl_name"

external get_decl_num_parameters : context -> func_decl -> int
	= "camlidl_z3_Z3_get_decl_num_parameters"

external get_decl_parameter_kind : context -> func_decl -> int -> parameter_kind
	= "camlidl_z3_Z3_get_decl_parameter_kind"

external get_decl_int_parameter : context -> func_decl -> int -> int
	= "camlidl_z3_Z3_get_decl_int_parameter"

external get_decl_double_parameter : context -> func_decl -> int -> float
	= "camlidl_z3_Z3_get_decl_double_parameter"

external get_decl_symbol_parameter : context -> func_decl -> int -> symbol
	= "camlidl_z3_Z3_get_decl_symbol_parameter"

external get_decl_sort_parameter : context -> func_decl -> int -> sort
	= "camlidl_z3_Z3_get_decl_sort_parameter"

external get_decl_ast_parameter : context -> func_decl -> int -> ast
	= "camlidl_z3_Z3_get_decl_ast_parameter"

external get_decl_func_decl_parameter : context -> func_decl -> int -> func_decl
	= "camlidl_z3_Z3_get_decl_func_decl_parameter"

external get_decl_rational_parameter : context -> func_decl -> int -> string
	= "camlidl_z3_Z3_get_decl_rational_parameter"

external get_sort_name : context -> sort -> symbol
	= "camlidl_z3_Z3_get_sort_name"

external get_sort : context -> ast -> sort
	= "camlidl_z3_Z3_get_sort"

external get_domain_size : context -> func_decl -> int
	= "camlidl_z3_Z3_get_domain_size"

external get_domain : context -> func_decl -> int -> sort
	= "camlidl_z3_Z3_get_domain"

external get_range : context -> func_decl -> sort
	= "camlidl_z3_Z3_get_range"

external get_sort_kind : context -> sort -> sort_kind
	= "camlidl_z3_Z3_get_sort_kind"

external get_bv_sort_size : context -> sort -> int
	= "camlidl_z3_Z3_get_bv_sort_size"

external get_array_sort_domain : context -> sort -> sort
	= "camlidl_z3_Z3_get_array_sort_domain"

external get_array_sort_range : context -> sort -> sort
	= "camlidl_z3_Z3_get_array_sort_range"

external get_tuple_sort_mk_decl : context -> sort -> func_decl
	= "camlidl_z3_Z3_get_tuple_sort_mk_decl"

external get_decl_kind : context -> func_decl -> decl_kind
	= "camlidl_z3_Z3_get_decl_kind"

external get_tuple_sort_num_fields : context -> sort -> int
	= "camlidl_z3_Z3_get_tuple_sort_num_fields"

external get_tuple_sort_field_decl : context -> sort -> int -> func_decl
	= "camlidl_z3_Z3_get_tuple_sort_field_decl"

external get_datatype_sort_num_constructors : context -> sort -> int
	= "camlidl_z3_Z3_get_datatype_sort_num_constructors"

external get_datatype_sort_constructor : context -> sort -> int -> func_decl
	= "camlidl_z3_Z3_get_datatype_sort_constructor"

external get_datatype_sort_recognizer : context -> sort -> int -> func_decl
	= "camlidl_z3_Z3_get_datatype_sort_recognizer"

external get_datatype_sort_constructor_accessor : context -> sort -> int -> int -> func_decl
	= "camlidl_z3_Z3_get_datatype_sort_constructor_accessor"

external get_pattern_num_terms : context -> pattern -> int
	= "camlidl_z3_Z3_get_pattern_num_terms"

external get_pattern : context -> pattern -> int -> ast
	= "camlidl_z3_Z3_get_pattern"

external simplify : context -> ast -> ast
	= "camlidl_z3_Z3_simplify"

external app_to_ast : context -> app -> ast
	= "camlidl_z3_Z3_app_to_ast"

external to_app : context -> ast -> app
	= "camlidl_z3_Z3_to_app"

external push : context -> unit
	= "camlidl_z3_Z3_push"

external pop : context -> int -> unit
	= "camlidl_z3_Z3_pop"

external persist_ast : context -> ast -> int -> unit
	= "camlidl_z3_Z3_persist_ast"

external assert_cnstr : context -> ast -> unit
	= "camlidl_z3_Z3_assert_cnstr"

external check_and_get_model : context -> lbool * model
	= "camlidl_z3_Z3_check_and_get_model"

external check : context -> lbool
	= "camlidl_z3_Z3_check"

external check_assumptions : context -> ast array -> int -> ast array -> lbool * model * ast * int * ast array
	= "camlidl_z3_Z3_check_assumptions"

external del_model : context -> model -> unit
	= "camlidl_z3_Z3_del_model"

external soft_check_cancel : context -> unit
	= "camlidl_z3_Z3_soft_check_cancel"

external get_search_failure : context -> search_failure
	= "camlidl_z3_Z3_get_search_failure"

external get_relevant_labels : context -> literals
	= "camlidl_z3_Z3_get_relevant_labels"

external get_relevant_literals : context -> literals
	= "camlidl_z3_Z3_get_relevant_literals"

external get_guessed_literals : context -> literals
	= "camlidl_z3_Z3_get_guessed_literals"

external del_literals : context -> literals -> unit
	= "camlidl_z3_Z3_del_literals"

external get_num_literals : context -> literals -> int
	= "camlidl_z3_Z3_get_num_literals"

external get_label_symbol : context -> literals -> int -> symbol
	= "camlidl_z3_Z3_get_label_symbol"

external get_literal : context -> literals -> int -> ast
	= "camlidl_z3_Z3_get_literal"

external disable_literal : context -> literals -> int -> unit
	= "camlidl_z3_Z3_disable_literal"

external block_literals : context -> literals -> unit
	= "camlidl_z3_Z3_block_literals"

external get_model_num_constants : context -> model -> int
	= "camlidl_z3_Z3_get_model_num_constants"

external get_model_constant : context -> model -> int -> func_decl
	= "camlidl_z3_Z3_get_model_constant"

external eval_func_decl : context -> model -> func_decl -> bool * ast
	= "camlidl_z3_Z3_eval_func_decl"

external is_array_value : context -> ast -> bool * int
	= "camlidl_z3_Z3_is_array_value"

external get_array_value : context -> ast -> ast array -> ast array -> ast array * ast array * ast
	= "camlidl_z3_Z3_get_array_value"

external get_model_num_funcs : context -> model -> int
	= "camlidl_z3_Z3_get_model_num_funcs"

external get_model_func_decl : context -> model -> int -> func_decl
	= "camlidl_z3_Z3_get_model_func_decl"

external get_model_func_else : context -> model -> int -> ast
	= "camlidl_z3_Z3_get_model_func_else"

external get_model_func_num_entries : context -> model -> int -> int
	= "camlidl_z3_Z3_get_model_func_num_entries"

external get_model_func_entry_num_args : context -> model -> int -> int -> int
	= "camlidl_z3_Z3_get_model_func_entry_num_args"

external get_model_func_entry_arg : context -> model -> int -> int -> int -> ast
	= "camlidl_z3_Z3_get_model_func_entry_arg"

external get_model_func_entry_value : context -> model -> int -> int -> ast
	= "camlidl_z3_Z3_get_model_func_entry_value"

external eval : context -> model -> ast -> bool * ast
	= "camlidl_z3_Z3_eval"

external open_log : context -> string -> bool
	= "camlidl_z3_Z3_open_log"

external close_log : context -> unit
	= "camlidl_z3_Z3_close_log"

external set_ast_print_mode : context -> ast_print_mode -> unit
	= "camlidl_z3_Z3_set_ast_print_mode"

external ast_to_string : context -> ast -> string
	= "camlidl_z3_Z3_ast_to_string"

external pattern_to_string : context -> pattern -> string
	= "camlidl_z3_Z3_pattern_to_string"

external sort_to_string : context -> sort -> string
	= "camlidl_z3_Z3_sort_to_string"

external func_decl_to_string : context -> func_decl -> string
	= "camlidl_z3_Z3_func_decl_to_string"

external model_to_string : context -> model -> string
	= "camlidl_z3_Z3_model_to_string"

external benchmark_to_smtlib_string : context -> string -> string -> string -> string -> int -> ast -> ast -> string
	= "camlidl_z3_Z3_benchmark_to_smtlib_string_bytecode" "camlidl_z3_Z3_benchmark_to_smtlib_string"

external context_to_string : context -> string
	= "camlidl_z3_Z3_context_to_string"

external parse_smtlib_string : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> unit
	= "camlidl_z3_Z3_parse_smtlib_string_bytecode" "camlidl_z3_Z3_parse_smtlib_string"

external parse_smtlib_file : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> unit
	= "camlidl_z3_Z3_parse_smtlib_file_bytecode" "camlidl_z3_Z3_parse_smtlib_file"

external get_smtlib_num_formulas : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_formulas"

external get_smtlib_formula : context -> int -> ast
	= "camlidl_z3_Z3_get_smtlib_formula"

external get_smtlib_num_assumptions : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_assumptions"

external get_smtlib_assumption : context -> int -> ast
	= "camlidl_z3_Z3_get_smtlib_assumption"

external get_smtlib_num_decls : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_decls"

external get_smtlib_decl : context -> int -> func_decl
	= "camlidl_z3_Z3_get_smtlib_decl"

external parse_z3_string : context -> string -> ast
	= "camlidl_z3_Z3_parse_z3_string"

external parse_z3_file : context -> string -> ast
	= "camlidl_z3_Z3_parse_z3_file"

external get_version : unit -> int * int * int * int
	= "camlidl_z3_Z3_get_version"

external reset_memory : unit -> unit
	= "camlidl_z3_Z3_reset_memory"




(* Internal auxillary functions: *)

(* Transform a pair of arrays into an array of pairs *)
let array_combine a b =
  if Array.length a <> Array.length b then raise (Invalid_argument "array_combine");
  Array.init (Array.length a) (fun i->(a.(i),b.(i)));;

(* [a |> b] is the pipeline operator for [b(a)] *)
let ( |> ) x f = f x;;


(* Extensions, except for refinement: *)
let mk_context_x configs = 
  let config = mk_config() in
  let f(param_id,param_value) = set_param_value config param_id param_value in
  Array.iter f configs;
  let context = mk_context config in
  del_config config;
  context;;

let get_app_args c a =
  Array.init (get_app_num_args c a) (get_app_arg c a);;

let get_domains c d =
  Array.init (get_domain_size c d) (get_domain c d);;

let get_array_sort c t = (get_array_sort_domain c t, get_array_sort_range c t);;

let get_tuple_sort c ty = 
  (get_tuple_sort_mk_decl c ty,
   Array.init (get_tuple_sort_num_fields c ty) (get_tuple_sort_field_decl c ty));;

type datatype_constructor_refined = { 
   constructor : func_decl; 
   recognizer : func_decl; 
   accessors : func_decl array 
}

let get_datatype_sort c ty = 
  Array.init (get_datatype_sort_num_constructors c ty)
  (fun idx_c -> 
   let constr = get_datatype_sort_constructor c ty idx_c in
   let recog = get_datatype_sort_recognizer  c ty idx_c in
   let num_acc = get_domain_size c constr in
   { constructor = constr;
     recognizer = recog;
     accessors = Array.init num_acc (get_datatype_sort_constructor_accessor c ty idx_c);
   })

let get_model_constants c m =
  Array.init (get_model_num_constants c m) (get_model_constant c m);;


let get_model_func_entry c m i j =
  (Array.init
     (get_model_func_entry_num_args c m i j)
     (get_model_func_entry_arg c m i j),
   get_model_func_entry_value c m i j);;

let get_model_func_entries c m i =
  Array.init (get_model_func_num_entries c m i) (get_model_func_entry c m i);;

let get_model_funcs c m =
  Array.init (get_model_num_funcs c m)
    (fun i->(get_model_func_decl c m i |> get_decl_name c,
             get_model_func_entries c m i,
             get_model_func_else c m i));;
 
let get_smtlib_formulas c = 
  Array.init (get_smtlib_num_formulas c) (get_smtlib_formula c);;

let get_smtlib_assumptions c = 
  Array.init (get_smtlib_num_assumptions c) (get_smtlib_assumption c);;

let get_smtlib_decls c =
  Array.init (get_smtlib_num_decls c) (get_smtlib_decl c);;

let get_smtlib_parse_results c =
  (get_smtlib_formulas c, get_smtlib_assumptions c, get_smtlib_decls c);;

let parse_smtlib_string_formula c a1 a2 a3 a4 a5 = 
  (parse_smtlib_string c a1 a2 a3 a4 a5;
   match get_smtlib_formulas c with [|f|] -> f | _ -> failwith "Z3: parse_smtlib_string_formula");;

let parse_smtlib_file_formula c a1 a2 a3 a4 a5 = 
  (parse_smtlib_file c a1 a2 a3 a4 a5;
   match get_smtlib_formulas c with [|f|] -> f | _ -> failwith "Z3: parse_smtlib_file_formula");;

let parse_smtlib_string_x c a1 a2 a3 a4 a5 = 
  (parse_smtlib_string c a1 a2 a3 a4 a5; get_smtlib_parse_results c);;

let parse_smtlib_file_x c a1 a2 a3 a4 a5 = 
  (parse_smtlib_file c a1 a2 a3 a4 a5; get_smtlib_parse_results c);;

(* Refinement: *)

type symbol_refined =
  | Symbol_int of int
  | Symbol_string of string
  | Symbol_unknown;;

let symbol_refine c s =
  match get_symbol_kind c s with
  | INT_SYMBOL -> Symbol_int (get_symbol_int c s)
  | STRING_SYMBOL -> Symbol_string (get_symbol_string c s);;

type sort_refined =
  | Sort_uninterpreted of symbol
  | Sort_bool
  | Sort_int
  | Sort_real
  | Sort_bv of int
  | Sort_array of (sort * sort)
  | Sort_datatype of datatype_constructor_refined array
  | Sort_unknown of symbol;;

let sort_refine c ty =
  match get_sort_kind c ty with
  | UNINTERPRETED_SORT -> Sort_uninterpreted (get_sort_name c ty)
  | BOOL_SORT -> Sort_bool
  | INT_SORT -> Sort_int
  | REAL_SORT -> Sort_real
  | BV_SORT -> Sort_bv (get_bv_sort_size c ty)
  | ARRAY_SORT -> Sort_array (get_array_sort_domain c ty, get_array_sort_range c ty)
  | DATATYPE_SORT -> Sort_datatype (get_datatype_sort c ty)
  | UNKNOWN_SORT -> Sort_unknown (get_sort_name c ty);;

let get_pattern_terms c p = 
  Array.init (get_pattern_num_terms c p) (get_pattern c p)

type binder_type = | Forall | Exists 

type numeral_refined = 
  | Numeral_small  of int64 * int64
  | Numeral_large  of string

type term_refined = 
  | Term_app        of decl_kind * func_decl * ast array
  | Term_quantifier of binder_type * int * ast array array * (symbol *sort) array * ast
  | Term_numeral    of numeral_refined * sort
  | Term_var        of int * sort

let term_refine c t = 
  match get_ast_kind c t with
  | NUMERAL_AST -> 
      let (is_small, n, d) = get_numeral_small c t in
      if is_small then 
	Term_numeral(Numeral_small(n,d), get_sort c t)
      else
	Term_numeral(Numeral_large(get_numeral_string c t), get_sort c t)
  | APP_AST   -> 
      let t' = to_app c t in
      let f =  get_app_decl c t' in
      let num_args = get_app_num_args c t' in
      let args = Array.init num_args (get_app_arg c t') in
      let k = get_decl_kind c f in
      Term_app (k, f, args)
  | QUANTIFIER_AST -> 
      let bt = if is_quantifier_forall c t then Forall else Exists in
      let w = get_quantifier_weight c t                            in
      let np = get_quantifier_num_patterns c t                     in
      let pats = Array.init np (get_quantifier_pattern_ast c t)    in
      let pats = Array.map (get_pattern_terms c) pats              in
      let nb = get_quantifier_num_bound c t                        in
      let bound = Array.init nb 
	  (fun i -> (get_quantifier_bound_name c t i, get_quantifier_bound_sort c t i)) in
      let body = get_quantifier_body c t in
      Term_quantifier(bt, w, pats, bound, body)
  | VAR_AST -> 
      Term_var(get_index_value c t, get_sort c t)
  | _ -> assert false



