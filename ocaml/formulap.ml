(** Printing formulas *)

module VH = Var.VarHash

module D = Debug.Make(struct let name = "Formulap" and default=`NoDebug end)
open D

open Ast
open Type

(** Returns a list of free variables in the given expression *)
let freevars e =
  let freevis =
    object(self)
      inherit Ast_visitor.nop
      val ctx = VH.create 570
      val found = VH.create 570

      method get_found =
	(* dprintf "found %d freevars" (VH.length found); *)
	List.rev (VH.fold (fun k () a -> k::a) found [])
      method add_dec d =
	if not(VH.mem found d || VH.mem ctx d)
	then VH.add found d ()
	(* else dprintf "Not adding %s." (Pp.var_to_string d) *)

      method visit_exp = function
	| Let(v, e1, e2) ->
	    ignore(Ast_visitor.exp_accept self e1);
	    VH.add ctx v ();
	    ignore(Ast_visitor.exp_accept self e2);
	    VH.remove ctx v;
	    SkipChildren
	| _ ->
	    DoChildren

      method visit_rvar r =
	self#add_dec r;
	DoChildren
    end
  in
  ignore(Ast_visitor.exp_accept freevis e);
  freevis#get_found

class virtual fpp =
object(self)
  method virtual forall : VH.key list -> unit
  method virtual ast_exp : Ast.exp -> unit
  method virtual assert_ast_exp : Ast.exp -> unit
  method virtual assert_ast_exp_with_foralls : ?fvars:bool -> VH.key list -> Ast.exp -> unit
  method virtual valid_ast_exp : ?exists:(var list) -> ?foralls:(var list) -> Ast.exp -> unit
  method virtual counterexample : unit
end

class virtual fpp_oc =
object(self)
  inherit fpp
  method virtual close : unit
  method virtual flush : unit
end

(* Naming this type is useful.

   I guess we should/could change the type of fpp_oc too to avoid
this. *)
type fppf = ?suffix:string -> out_channel -> fpp_oc


class virtual stream_fpp =
object(self)
  method virtual andstart : unit -> unit
  method virtual andend : unit -> unit
  method virtual letmebegin : var -> Ast.exp -> unit
  method virtual letmeend : var -> unit
  method virtual open_benchmark_has_mem : unit -> unit
  method virtual close_benchmark : unit -> unit
  method virtual print_assertion : Ast.exp -> unit
  method virtual declare_given_freevars : var list -> unit
  method virtual declare_new_free_var : var -> unit
  method virtual print_free_var : var -> unit
  method virtual formula : unit -> unit
end

class virtual stream_fpp_oc =
object(self)
  inherit stream_fpp
  method virtual close : unit
  method virtual flush : unit
end

type double_printer_type = {formula_p : stream_fpp_oc;
                            free_var_p : stream_fpp_oc}

