module type G =
sig
  include Graph.Sig.I

  val remove_edge_e : t -> E.t -> unit
  val v2s : V.t -> string
end

(* body is a superset of headers. *)
type 'a lnt = { headers: 'a list; body: 'a list; children: 'a lnf }
and 'a lnf = 'a lnt list

let rec is_sorted l =
  match l with
  | [] -> true
  | [v] -> true
  | u::v::rest -> u <= v && is_sorted (v::rest)

(* invariant: sub is sorted and sup is sorted. *)
let rec is_subset sub sup =
  match sub, sup with
  | [], _ -> true
  | x::_, [] -> false
  | x::subrest, y::suprest when x = y -> is_subset subrest suprest
  | x::subrest, y::suprest when x > y -> false
  | _, y::suprest -> is_subset sub suprest

let rec validate_lnf lnf =
  is_sorted lnf && List.fold_left (fun p lnt -> p && validate_lnt lnt) true lnf
and validate_lnt lnt =
  is_sorted lnt.headers
  && is_sorted lnt.body
  && is_subset lnt.headers lnt.body
  && validate_lnf lnt.children

let rec string_of_list print_fun l =
  match l with
  | [] -> ""
  | [v] -> print_fun v
  (* TODO(awreece) Use ocaml list syntax? *)
  | v::rest -> (print_fun v) ^ ", " ^ string_of_list print_fun rest

let rec string_of_lnf print_fun lnf =
  match lnf with
  | [] -> ""
  | _ -> "LNF(" ^ (string_of_list (string_of_lnt print_fun) lnf) ^ ")"
and string_of_lnt print_fun lnt =
  "LNT(headers=" ^ (string_of_list print_fun lnt.headers) ^
  "; body=" ^ (string_of_list print_fun lnt.body) ^
  "; children=" ^ (string_of_lnf print_fun lnt.children) ^
  ")"

module type MakeType =
  functor (Gr: G) ->
    sig
      val lnf : Gr.t -> Gr.V.t lnf
    end
