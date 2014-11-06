open Unix
open Buffer
open Merlin_NFA
open Merlin_Util
open Merlin_Types
open Merlin_Globals
open Merlin_Error


module Sat = struct
  type zVar = string

  type zSort =
    | SInt
    | SPacket

  type zTerm =
    | TVar of zVar
    | TInt of Int64.t
    | TApp of zVar * zTerm

  type zFormula =
    | ZTrue
    | ZFalse
    | ZNot of zFormula
    | ZAnd of zFormula list
    | ZOr of zFormula list
    | ZEquals of zTerm * zTerm

  type zDeclaration =
    | ZVarDeclare of zVar * zSort
    | ZSortDeclare of zVar * (zVar * (zVar * zSort) list) list
    | ZAssertDeclare of zFormula

  type zProgram =
    | ZProgram of zDeclaration list

  let fresh_cell = ref []

  let reset_fresh () =
    fresh_cell := []

  let fresh sort =
    let l = !fresh_cell in
    let n = List.length l in
    let x = match sort with
      | SPacket -> Printf.sprintf "_p%d" n
      | SInt -> Printf.sprintf "_n%d" n in
    fresh_cell := ZVarDeclare(x,sort)::l;
    x

  let rec serialize_sort = function
    | SPacket ->
      "Packet"
    | SInt ->
      "Int"

  let buffer_sort s buf = match s with
    | SPacket -> add_string buf "Packet"
    | SInt -> add_string buf "Int"

  let rec serialize_term term : string =
    match term with
    | TVar x ->
      x
    | TInt n ->
      Printf.sprintf "%s" (Int64.to_string n)
    | TApp (f, term) ->
      Printf.sprintf "(%s %s)" f (serialize_term term)

  let rec buffer_term term buf = match term with
    | TVar x -> add_string buf x
    | TInt n -> add_string buf (Int64.to_string n)
    | TApp (f, term) -> add_string buf "("; add_string buf f; add_string buf " ";
      buffer_term term buf; add_string buf ")"

  let rec serialize_formula = function
    | ZTrue ->
      Printf.sprintf "true"
    | ZFalse ->
      Printf.sprintf "false"
    | ZNot f1 ->
      Printf.sprintf "(not %s)" (serialize_formula f1)
    | ZEquals (t1, t2) ->
      Printf.sprintf "(equals %s %s)" (serialize_term t1) (serialize_term t2)
    | ZAnd([]) ->
      Printf.sprintf "true"
    | ZAnd([f]) ->
      Printf.sprintf "%s" (serialize_formula f)
    | ZAnd(f::fs) ->
      Printf.sprintf "(and %s %s)"
    (serialize_formula f) (serialize_formula (ZAnd(fs)))
    | ZOr([]) ->
      Printf.sprintf "false"
    | ZOr([f]) ->
      Printf.sprintf "%s" (serialize_formula f)
    | ZOr(f::fs) ->
      Printf.sprintf "(or %s %s)"
    (serialize_formula f) (serialize_formula (ZOr(fs)))


  let rec buffer_formula f buf = match f with
    | ZTrue ->
      add_string buf "true"
    | ZFalse ->
      add_string buf "false"
    | ZNot f1 ->
      add_string buf "(not "; buffer_formula f1 buf; add_string buf ")"
    | ZEquals (t1, t2) ->
      add_string buf "(equals "; buffer_term t1 buf; buffer_term t2 buf; add_string buf ")"
    | ZAnd([]) ->
      add_string buf "true"
    | ZAnd([f]) ->
      buffer_formula f buf;
    | ZAnd(f::fs) ->
      add_string buf "(and ";
      buffer_formula f buf; buffer_formula (ZAnd(fs)) buf; add_string buf ")"
    | ZOr([]) ->
      add_string buf "false"
    | ZOr([f]) ->
      buffer_formula f buf
    | ZOr(f::fs) ->
      add_string buf "(or ";
      buffer_formula f buf; buffer_formula (ZOr(fs)) buf; add_string buf ")"


  let serialize_declaration = function
    | ZSortDeclare (name, constructorList) ->
      let serialize_field (field,sort) =
        Printf.sprintf "(%s %s)" field (serialize_sort sort) in
      let serialize_constructor (name, fields) =
        Printf.sprintf "(%s %s)" name
      (intercalate serialize_field " " fields) in
      Printf.sprintf "(declare-datatypes () ((%s %s)))"
        name (intercalate serialize_constructor " " constructorList)
    | ZVarDeclare (x, sort) ->
      Printf.sprintf "(declare-var %s %s)" x (serialize_sort sort)
    | ZAssertDeclare(f) ->
      Printf.sprintf "(assert %s)" (serialize_formula f)

  let buffer_declaration d buf = match d with
    | ZSortDeclare (name, constructorList) ->
      let buffer_field (field,sort) buf =
        add_string buf "("; add_string buf field; buffer_sort sort buf; add_string buf ")" in
      let buffer_constructor (name, fields) buf =
        add_string buf "("; add_string buf name;
        buffer_intercalate buf buffer_field " " fields;
        add_string buf ")" in
      add_string buf "(declare-datatypes () (("; add_string buf name;
      add_string buf " ";
      buffer_intercalate buf buffer_constructor " " constructorList;
      add_string buf ")))"
    | ZVarDeclare (x,sort) ->
      add_string buf "(declare-var "; add_string buf x; buffer_sort sort buf; add_string buf ")"
    | ZAssertDeclare(f) ->
      add_string buf "(assert "; buffer_formula f buf; add_string buf ")"

  let init_decls : zDeclaration list =
    [ ZSortDeclare
        ("Packet",
     [("packet",
       [ ("Switch", SInt)
       ; ("EthDst", SInt)
       ; ("EthType", SInt)
       ; ("Vlan", SInt)
       ; ("VlanPcp", SInt)
       ; ("IPProto", SInt)
       ; ("IP4Src", SInt)
       ; ("IP4Dst", SInt)
       ; ("TCPSrcPort", SInt)
       ; ("TCPDstPort", SInt)
       ; ("EthSrc", SInt)
       ; ("InPort", SInt)])])]

  let serialize_program (ZProgram decls) =
    let buf = Buffer.create 1000 in
    buffer_intercalate buf buffer_declaration "\n" init_decls;
    buffer_intercalate buf buffer_declaration "\n" (!fresh_cell);
    buffer_intercalate buf buffer_declaration "\n" decls;
    add_string buf "\n(check-sat)";
    contents buf

  let solve prog : bool =
    let s = serialize_program prog in
    let z3_out,z3_in = try open_process "z3 -in -smt2 -nw"
    with Unix_error (e,f,a) ->
      Printf.printf "open: %s|%s|%s\n" (error_message e) f a; exit 1 in
    let _ = try output_string z3_in s
    with Unix_error (e,f,a) ->
      Printf.printf "out: %s|%s|%s\n" (error_message e) f a; exit 1 in
    let _ = try flush z3_in
    with Unix_error (e,f,a) ->
      Printf.printf "flush: %s|%s|%s\n" (error_message e) f a; exit 1 in
    let _ = try close_out z3_in
    with Unix_error (e,f,a) ->
      Printf.printf "close: %s|%s|%s\n" (error_message e) f a ; exit 1 in
    let b = Buffer.create 17 in
    (try
       while true do
         Buffer.add_string b (input_line z3_out);
         Buffer.add_char b '\n';
       done
     with
       | End_of_file -> ()
       | Unix_error (e,f,a) -> Printf.printf "open: %s|%s|%s\n" (error_message e) f a
    );
    let _ = close_in z3_out in
    Buffer.contents b = "sat\n"
end

(* cnf predicate *)
type cnf =
    { dlSrc : (bool * Int64.t) option;
      dlDst : (bool * Int64.t) option;
      dlType : (bool * Int64.t) option;
      dlVlan : (bool * Int64.t) option;
      dlVlanPcp: (bool * Int64.t) option;
      nwProto : (bool * Int64.t) option;
      nwSrc : (bool * Int64.t) option;
      nwDst : (bool * Int64.t) option;
      tpSrc : (bool * Int64.t) option;
      tpDst : (bool * Int64.t) option }

type annot =
    { mutable cnf : (cnf option) option;
      mutable nfa : Merlin_NFA.NFA.t option }


module DecidePred = struct
  open Packet
  open Sat


  let pred_empty (pr:Merlin_Types.pred) : bool =
    let rec loop (pr:Merlin_Types.pred) (pkt:zVar) : zFormula =
      match pr with
      | Test(f, v) ->
        let f = match f with 
          | SDN_Types.InPort -> "InPort"
          | SDN_Types.EthType -> "EthType"
          | SDN_Types.EthSrc -> "EthSrc"
          | SDN_Types.EthDst -> "EthDst"
          | SDN_Types.Vlan -> "Vlan"
          | SDN_Types.VlanPcp -> "VlanPcp"
          | SDN_Types.IPProto -> "IPProto"
          | SDN_Types.IP4Src -> "IP4Src"
          | SDN_Types.IP4Dst -> "Ip4Dst"
          | SDN_Types.TCPSrcPort -> "TCPSrcPort"
          | SDN_Types.TCPDstPort -> "TCPDstPort" in 
        ZEquals(TApp(f, TVar pkt), TInt(VInt.get_int64 v))
      | Switch(s) ->
        ZEquals(TApp("Switch", TVar pkt), TInt(VInt.get_int64 s))
      | Or(p1,p2) ->
        ZOr [loop p1 pkt; loop p2 pkt]
      | And(p1,p2) ->
        ZAnd [loop p1 pkt; loop p2 pkt]
      | Not(p1) ->
        ZNot(loop p1 pkt)
      | Everything ->
        ZTrue
      | Nothing ->
        ZFalse in
    let phi = loop pr (fresh SPacket) in
    let b = solve (ZProgram [ZAssertDeclare phi]) in
    reset_fresh ();
    not b
end

(* module Cnf = struct *)

(*   exception Empty_cnf *)

(*   let pat_to_cnf pat = *)
(*     let open Merlin_Wildcard in *)
(*     let f c = function *)
(*       | WildcardAll -> None *)
(*       | WildcardExact v -> Some (true, c v) *)
(*       | WildcardNone -> raise Empty_cnf in *)
(*     let f64 = f (fun x -> x) in *)
(*     let f32 = f Int64.of_int32 in *)
(*     let f16 = f Int64.of_int in *)
(*     let f16o = f (fun o -> match o with None -> Int64.of_int 0xffff | Some v -> Int64.of_int v) in *)
(*     try *)
(*       Some { dlSrc = f64 pat.ptrnDlSrc; *)
(*              dlDst = f64 pat.ptrnDlDst; *)
(*              dlType = f16 pat.ptrnDlTyp; *)
(*              dlVlan = f16o pat.ptrnDlVlan; *)
(*              dlVlanPcp = f16 pat.ptrnDlVlanPcp; *)
(*              nwProto = f16 pat.ptrnNwProto; *)
(*              nwSrc = f32 pat.ptrnNwSrc; *)
(*              nwDst = f32 pat.ptrnNwDst; *)
(*              tpSrc = f16 pat.ptrnTpSrc; *)
(*              tpDst = f16 pat.ptrnTpDst } *)
(*     with Empty_cnf -> None *)

(*   let all_cnf = *)
(*     { dlSrc = None; *)
(*       dlDst = None; *)
(*       dlType = None; *)
(*       dlVlan = None; *)
(*       dlVlanPcp = None; *)
(*       nwProto = None; *)
(*       nwSrc = None; *)
(*       nwDst = None; *)
(*       tpSrc = None; *)
(*       tpDst = None } *)

(*   let and_cnf c1 c2 = *)
(*     try *)
(*       let m o1 o2 = match o1,o2 with *)
(*         | None,_ -> o2 *)
(*         | _,None -> o1 *)
(*         | Some (true,v1), Some (false, v2) when v1 <> v2 -> *)
(*           Some (true,v1) *)
(*         | Some (false,v1), Some (true, v2) when v1 <> v2 -> *)
(*           Some (true, v2) *)
(*         | Some (true,v1), Some (true,v2) when v1 = v2 -> *)
(*           Some (true, v1) *)
(*         | _ -> raise Empty_cnf in *)
(*       Some { dlSrc = m c1.dlSrc c2.dlSrc; *)
(*              dlDst = m c1.dlDst c2.dlDst; *)
(*              dlType = m c1.dlType c2.dlType; *)
(*              dlVlan = m c1.dlVlan c2.dlVlan; *)
(*              dlVlanPcp = m c1.dlVlanPcp c2.dlVlanPcp; *)
(*              nwProto = m c1.nwProto c2.nwProto; *)
(*              nwSrc = m c1.nwSrc c2.nwSrc; *)
(*              nwDst = m c1.nwDst c2.nwDst; *)
(*              tpSrc = m c1.tpSrc c2.tpSrc; *)
(*              tpDst = m c1.tpDst c2.tpDst; *)
(*            } *)
(*     with Empty_cnf -> None *)

(*   let inter_cnf c1 c2 = *)
(*     let m o1 o2 = match o1,o2 with *)
(*       | None,_ | _, None -> true *)
(*       | Some (true,v1), Some (false, v2) when v1 <> v2 -> true *)
(*       | Some (false,v1), Some (true, v2) when v1 <> v2 -> true *)
(*       | Some (true,v1), Some (true,v2) when v1 = v2 -> true *)
(*       | Some (false,v1), Some (false,v2) -> true (\* TODO(jnf): as long as there are >= 2 values *\) *)
(*       | _ -> false in *)
(*     (m c1.dlSrc c2.dlSrc && *)
(*      m c1.dlDst c2.dlDst && *)
(*      m c1.dlType c2.dlType && *)
(*      m c1.dlVlan c2.dlVlan && *)
(*      m c1.dlVlanPcp c2.dlVlanPcp && *)
(*      m c1.nwProto c2.nwProto && *)
(*      m c1.nwSrc c2.nwSrc && *)
(*      m c1.nwDst c2.nwDst && *)
(*      m c1.tpSrc c2.tpSrc && *)
(*      m c1.tpDst c2.tpDst) *)

(*   let neg_cnf c1 = *)
(*     None *)

(*   let rec pred_cnf pr = *)
(*     match pr with *)
(*       | Hdr(pat) -> *)
(*         pat_to_cnf pat *)
(*       | OnSwitch(s) -> *)
(*         None *)
(*       | Or(_,_) -> *)
(*         None *)
(*       | And(pr1,pr2) -> *)
(*         begin match pred_cnf pr1, pred_cnf pr2 with *)
(*           | Some c1, Some c2 -> and_cnf c1 c2 *)
(*           | _ -> None *)
(*         end *)
(*       | Not(pr1) -> *)
(*         begin match pred_cnf pr1 with *)
(*           | Some c1 -> neg_cnf c1 *)
(*           | None -> None *)
(*         end *)
(*       | Everything -> *)
(*         Some all_cnf *)
(*       | Nothing -> *)
(*         None *)
(* end *)

(* let get_cnf s = *)
(*   let Statement(_,pr,_,_) = s in *)
(*   let o = Cnf.pred_cnf pr in o *)

let get_nfa s =
  let PolicyIR(_,rx,_) = s in
  let m = NFA.of_regex rx in m

(* Either the statements deal with different traffic, or the first
   implies the second *)
let overlap (PolicyIR(pr1,rx1,bw1) as s1) (PolicyIR(pr2,rx2,bw2) as s2) =
  let pred_disjoint () =
    (* match get_cnf s1, get_cnf s2 with *)
    (*   | Some c1, Some c2 -> *)
    (*     not (Cnf.inter_cnf c1 c2) *)
    (*   | _ -> *)
        DecidePred.pred_empty (And(pr1,pr2)) in
  let regex_include () =
    let m1 = get_nfa s1 in
    let m2 = get_nfa s2 in
    NFA.subseteq m1 m2 in
  if pred_disjoint () then
    Some (Rate(0L,0L))
  else if regex_include () then
    Some bw2
  else
    None

(****** Functions that will be exposed ******)
let is_empty (stmt:policy_ir) : bool =
    let PolicyIR(pred,_,_) = stmt in
    DecidePred.pred_empty pred

let denullify (pol:program_ir) : program_ir =
  let ProgramIR(stmts) = pol in
  let stmts' = List.filter (fun s -> not (is_empty s)) stmts in
  ProgramIR(stmts')

(* Restrict a policy by a predicate *)
let restrict (pol:program_ir) (pd:pred) : program_ir =
  let ProgramIR(stmts) = pol in
  let stmts' = List.map
    (fun s -> match s with
      | PolicyIR(pd',rx,rate) ->
        PolicyIR(And(pd, pd'), rx, rate)
    ) stmts in
  ProgramIR(stmts')

(* Combine two policies into one, assuming that all predicates between both
   policies are disjoint *)
let disjoint_union (p1:program_ir) (p2:program_ir) : program_ir =
  let ProgramIR(stmts1) = p1 in
  let ProgramIR(stmts2) = p2 in
  ProgramIR(stmts1 @ stmts2)

(* Does p1 imply p2? *)
let implies (p1:program_ir) (p2:program_ir) : bool =
  (* let ProgramIR(stmts1) = p1 in *)
  (* let ProgramIR(stmts2) = p2 in *)
  (* TODO(rjs) Merlin_Policy implies not implemented *)
  Printf.printf "TODO: Merlin_Policy implies not implemented\n";
  raise Not_Implemented;
  false
    
(*
  List.fold_left
    (fun ok (PolicyIR(_,_,bw) as stmt) ->
      ok &&
        (match
            List.fold_left
          (fun bwo stmt' ->
                match bwo with
                  | None -> None
                  | Some bw ->
                    match overlap stmt stmt' with
                      | None -> None
                      | Some bw' ->
                        match bw, bw' with
                          | Rate(min1,max1), Rate(min2,max2) ->
                            let max' = Int64.sub max1 max2 in
                            let min' = Int64.sub min1 min2 in
                            if Int64.compare max' 0L >= 0 &&
                              Int64.compare min' 0L >= 0 then
                              Some (Rate(min', max'))
                            else
                              None)
              (Some bw) stmts2
         with
           | None -> false
           | Some _ -> true))
    true stmts1
*)
