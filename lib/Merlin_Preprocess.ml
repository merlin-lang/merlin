
open Merlin_Globals
open Merlin_Pretty
open Merlin_Types
open Merlin_Dictionaries
open Merlin_Error
open Merlin_NFA
open Frenetic_OpenFlow

module NK = Frenetic_NetKAT

(* Bookkeeping for Presburger variable generation *)
let var_num = ref 0

let fresh_var () : int =
  incr var_num ; !var_num

let new_var () =
  let n = fresh_var () in
  Printf.sprintf "v%d" n


(** Source to Source expansion **)

let cross (s1:LocationSet.t) (s2:LocationSet.t) : (string * string) list =
  let crossed = LocationSet.fold (fun l1 acc ->
    LocationSet.fold (fun l2 acc ->
      (l1,l2)::acc) s2 acc) s1 [] in
    List.rev crossed

let distinct (s1:LocationSet.t) (s2:LocationSet.t) : (string * string) list =
  let crossed = LocationSet.fold (fun l1 acc ->
    LocationSet.fold (fun l2 acc ->
      if l1 = l2 then acc
      else (l1,l2)::acc) s2 acc) s1 [] in
  List.rev crossed

let zip (s1:LocationSet.t) (s2:LocationSet.t) : (string * string) list =
  List.combine (LocationSet.elements s1) (LocationSet.elements s2)

let expand id s1 s2 = match id with
  | "cross" -> cross s1 s2
  | "distinct" -> distinct s1 s2
  | "zip" -> zip s1 s2
  | _ -> raise (Unknown_function(id))

let pairs (ASTPolicy(c,p,r,ro)) =
  let Foreach(pair,expansion) = c in
  let Expansion(id,s1,s2) = expansion in
  expand id s1 s2
  (* match expr with *)
  (* | Call(id, s1, s2) -> expand id s1 s2 *)
  (* | _ ->  raise (Unknown_function("expected call expression")) *)

(* Pad the various datatypes as necessary *)
let pad_regex (r:regex) src dst : regex =
  Cat(Cat(Char src, r),
      Char(dst))

let pad_predicate pred src dst =
  let sloc = LocationHash.find location_to_addr src in
  let dloc = LocationHash.find location_to_addr dst in
  let s = Test (NK.IP4Src(sloc,Int32.of_int 32)) in
  let d = Test (NK.IP4Dst(dloc,Int32.of_int 32)) in
  And(And (s, d), pred)

let pad_stmt (s:statement) : statement =
  let Statement(p,r,v) = s in
  let r' = pad_regex r start_symbol end_symbol in
  Statement(p,r',v)

let pad_ir (Policy(statements, formula):policy) =
  let statements' = List.map pad_stmt statements in
  Policy(statements', formula)

(** Desugar the AST into the IR **)

let desugar (ast:ast_program) : policy option =
  let ASTProgram(policies) = ast in
  let statements, formula =
    List.fold_left (fun (ss,fs) pol ->
      let hosts = pairs pol in
      let ASTPolicy(comp,pred,r,rate) = pol in

      let vs = List.map (fun _ -> new_var ()) hosts in
      let sum = List.fold_left (fun acc v ->
        match acc with
        | BLit(0L) ->  BVar(v)
        | _ -> BSum(acc, BVar(v))) (BLit(0L)) vs in

      let new_statements =
        List.map2 (fun v (s,d) ->
          let src = get_symbol (s,None) in
          let dst = get_symbol (d,None) in
          let r' = pad_regex r src dst in
          add_externals v s d;
          let pred' = pad_predicate pred s d in
          Statement(pred',r',v) ) vs hosts
      in

      let term = match rate with
        | RNone -> FNone
        | RMin(n) -> FMin( sum, n)
        | RMax(n) -> FMax( sum, n)
        | RBoth(n, m) ->  FAnd(FMin(sum, n),  FMax( sum, m))
      in
      let fs' =
        match fs with
        | FNone -> term
        | _ ->  FAnd(term, fs)
      in

      (new_statements@ss, fs')
    ) ([], FNone) policies in

  if List.length statements = 0 then None
  else Some (Policy(statements, formula))


(* Localize the Presburger formulae, if possible *)
let rec vars b acc =
  match b with
  | BLit(_) -> acc
  | BVar(s) -> BVar(s)::acc
  | BSum(b1,b2) -> let acc' = (vars b1 acc) in vars b2 acc'

let localize_min b n =
  let vs = vars b [] in
  let rate =  (Int64.div n ( Int64.of_int (List.length vs))) in
  List.fold_left (fun acc v ->
    let term = FMin(v, rate) in
    match acc with
    | FNone -> term
    | _ ->  FAnd(acc,term)) FNone vs

let localize_max b n =
  let vs = vars b [] in
  let rate =  (Int64.div n ( Int64.of_int (List.length vs))) in
  List.fold_left (fun acc v ->
    let term = FMax(v, rate) in
    match acc with
    | FNone -> term
    | _ ->  FAnd(acc,term)) FNone vs

let rec localize_formula (formula:formula) : formula =
  match formula with
  | FMin(b, n) -> (localize_min b n)
  | FMax(b, n) -> (localize_max b n)
  | FAnd(f1,f2) -> FAnd((localize_formula f1), (localize_formula f2))
  | FOr(f1,f2) -> FOr((localize_formula f1), (localize_formula f2))
  | FNeg(f) -> FNeg((localize_formula f))
  | FNone -> formula

let localize_ir (Policy(statements, formula):policy) : policy =
  let formula' = localize_formula formula in
  Policy(statements, formula')
