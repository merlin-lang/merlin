open Merlin_Globals
open Merlin_Pretty
open Merlin_Types
open Merlin_Dictionaries
open Merlin_Error
open Merlin_NFA
open Frenetic_OpenFlow

module NK = Frenetic_NetKAT

(** Pad the various datatypes as necessary *)
let pad_regex (r:regex) src dst : regex =
  Cat(Cat(Char src, r), Char(dst))

let pad_predicate pred src dst =
  let sloc = LocationHash.find location_to_addr src in
  let dloc = LocationHash.find location_to_addr dst in
  let s = Test (NK.IP4Src(sloc,Int32.of_int 32)) in
  let d = Test (NK.IP4Dst(dloc,Int32.of_int 32)) in
  And(And (s, d), pred)

let pad_stmt (s:statement) : statement =
  let Statement(v,p,r) = s in
  let r' = pad_regex r start_symbol end_symbol in
  Statement(v,p,r')

let pad_policy (Policy(statements, formula):policy) =
  let statements' = List.map pad_stmt statements in
  Policy(statements', formula)


(** Localize the Presburger formulae, if possible *)
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

let localize (Policy(statements, formula):policy) : policy =
  let formula' = localize_formula formula in
  Policy(statements, formula')


(** Generate pairs of sources and destinations based on sets of locations *)
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


(** Bookkeeping for Presburger variable generation *)
let var_num = ref 0

let fresh_var () : int =
  incr var_num ; !var_num

let rec new_var set =
  let n = fresh_var () in
  let v = Printf.sprintf "v%d" n in
  if StringSet.mem v set then new_var set else StringSet.add v set, v

(** Desugar the macro language into the core language **)
let rec expand_container names container = match container with
  | Name n ->
    ( try StringHash.find names n
    with Not_found -> LocationSet.singleton n )
  | Set s ->
    LocationSet.fold
      (fun l acc -> LocationSet.union acc (expand_container names (Name l)))
      s (LocationSet.empty)

let rec expand_regex names (r:regex) = match r with
  | Char c ->
    ( try
      let name = SymbolHash.find symbol_to_location c in
      let locs = StringHash.find names name in
      LocationSet.fold (fun l acc -> match acc with
          | Empty -> Char(get_symbol l)
          | _ -> Alt(acc, Char(get_symbol l))) locs Empty
    with Not_found -> Char c )
  | Alt (r1,r2) -> Alt(expand_regex names r1, expand_regex names r2)
  | Cat (r1,r2) -> Cat(expand_regex names r1, expand_regex names r2)
  | Kleene r -> Kleene (expand_regex names r)
  | Group r -> Group (expand_regex names r)
  | Empty -> Empty
  | AnyChar -> AnyChar

let expand_block names vars (Block block) =
  let Foreach(_,_) = block.iter in
  let ex = expand_container names in
  let ends = match block.expander with
    | Zip(s,d)      -> zip (ex s) (ex d)
    | Cross(s,d)    -> cross (ex s) (ex d)
    | Distinct(s,d) -> distinct (ex s) (ex d) in

  let _,vars = List.fold_left (fun ( acc, vars ) _ ->
      let acc', v = new_var acc in acc', v::vars) (vars,[]) ends in
  let sum = List.fold_left (fun acc v ->
      match acc with
      | BLit(0L) ->  BVar(v)
      | _ -> BSum(acc, BVar(v))) (BLit(0L)) vars in

  let stmts =
    List.map2 (fun v (s,d) ->
        let src = get_symbol s in
        let dst = get_symbol d in
        let r' = pad_regex block.regex src dst in
        add_externals v s d;
        let pred' = pad_predicate block.pred s d in
        Statement(v,pred',r') ) vars ends
  in

  let formula = match block.rate with
    | RNone -> FNone
    | RMin(n) -> FMin( sum, n)
    | RMax(n) -> FMax( sum, n)
    | RBoth(n, m) ->  FAnd(FMin(sum, n),  FMax( sum, m))
  in
  Policy(stmts, formula)

let expand (Program pgm) =
  let vs = List.fold_left (fun acc ( Policy(s,_) ) ->
      List.fold_left (fun acc ( Statement(v,_,_)) -> StringSet.add v acc) acc s)
      StringSet.empty pgm.policies in
  let pols = pgm.policies in
  (* Expand macro blocks into policies *)
  let pols' = List.map (expand_block pgm.names vs) pgm.blocks in

  (* Expand the regular expressions using the name to set mappings *)
  List.map (fun (Policy(stmts,form)) ->
      let stmts' = List.map (fun (Statement(v,p,r)) ->
          Statement(v,p, expand_regex pgm.names r)) stmts in
      Policy(stmts',form)) (pols@pols')

let partition policies =
  List.partition
    (fun (Policy(_,f)) -> match f with FNone -> true | _ -> false)
    policies

let flatten policies =
  let s, f = List.fold_left (fun (stmts,f) (Policy(stmts',f')) ->
      stmts@stmts',
      match f,f' with
      | FNone, FNone -> FNone
      | FNone,f
      | f,FNone -> f
      | f1,f2 -> FAnd(f1,f2)
    ) ([],FNone) policies in
  Policy(s,f)

