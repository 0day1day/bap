(** Function boundary identification for x86

    @author Tiffany (Youzhi) Bao
*)
open Asmir
open Type
open Ast

type scheme_type = 
    | RECURSIVE_DESCENT 
    | VSA

(** [start_addresses p] identifies a list of function start addresses
    in [p] using heuristics. Raises [Invalid_argument] if called on a
    non-x86 program. *)
val start_addresses : asmprogram -> addr list

(** [end_address_at p addr scheme] returns the identified end address
    of the function starting at [addr] in [p] using the CFG recovery
    scheme [scheme]. Currently we set 30 seconds as the maximuim time
    for building up the control flow graph. If timeout, the address 
    would be None. *)
val end_address_at : asmprogram -> addr -> scheme_type -> addr option

(** [get_function_ranges p] finds functions using the symbol table,
    and if that fails, uses [start_addresses] to identify funtions. *)
val get_function_ranges : asmprogram -> (string * addr * addr) list
