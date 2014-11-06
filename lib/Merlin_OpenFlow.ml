open SDN_Types
open Pattern 
open Merlin_Types
open Merlin_Error

module NK = NetKAT_Types

type pattern = Pattern.t
let all_pattern = match_all

exception Empty_pat

let mk_flow (pat:pattern) (group:group) : SDN_Types.flow =
  { pattern = pat;
    action = group;
    cookie = 0L;
    idle_timeout = Permanent;
    hard_timeout = Permanent }

let group_union (g1:group) (g2:group) : group =
  match g1, g2 with
    | [s1],[s2] ->
      [s1 @ s2]
    | [], [s] -> [s]
    | [s], [] -> [s]
    | _ ->
      raise (Impossible_group_union (g1,g2))

(*  Intersect two patterns. If either is non-empty, then both must be the same *)

(* JNF: "must both the the same"? This is not what set-theoretic
   intersection means... *)
let pat_inter (pat1:pattern) (pat2:pattern) =
  let f vo1 vo2 = match vo1,vo2 with
    | Some v1, Some v2 ->
      if v1 = v2 then Some v1
      else raise Empty_pat
    | Some v1, None ->
      Some v1
    | None, Some v2 ->
      Some v2
    | None, None -> 
      None in 
  try
    Some
      { dlSrc = f pat1.dlSrc pat2.dlSrc
      ; dlDst = f pat1.dlDst pat2.dlDst
      ; dlTyp  = f pat1.dlTyp pat2.dlTyp
      ; dlVlan = f pat1.dlVlan pat2.dlVlan
      ; dlVlanPcp  = f pat1.dlVlanPcp pat2.dlVlanPcp
      ; nwSrc  = f pat1.nwSrc pat2.nwSrc
      ; nwDst  = f pat1.nwDst pat2.nwDst
      ; nwProto  = f pat1.nwProto pat2.nwProto
      ; tpSrc  = f pat1.tpSrc pat2.tpSrc
      ; tpDst  = f pat1.tpSrc pat2.tpDst
      ; inPort  = f pat1.inPort pat2.inPort }
  with  Empty_pat ->
    None

(* Two patterns are equal if they both have the same fields and the values for
   present fields are the same *)
(* JNF: not equal; subset or equal *)
let pat_subseteq (pat1:pattern) (pat2:pattern) =
  (* Printf.printf "Calling pat_subseteq\n%!"; *)
  let f vo1 vo2 = 
    match vo1,vo2 with
      | _,None -> true
      | None,_ -> false
      | Some v1, Some v2 -> v1 = v2 in 
     (  f pat1.dlSrc pat2.dlSrc
     && f pat1.dlDst pat2.dlDst
     && f pat1.dlTyp pat2.dlTyp
     && f pat1.dlVlan pat2.dlVlan
     && f pat1.dlVlanPcp pat2.dlVlanPcp
     && f pat1.nwSrc pat2.nwSrc
     && f pat1.nwDst pat2.nwDst
     && f pat1.nwProto pat2.nwProto
     && f pat1.tpSrc pat2.tpSrc
     && f pat1.tpSrc pat2.tpDst
     && f pat1.inPort pat2.inPort )

(* Deduplicate flowtable entries with subseteq patterns *)
let optimize (t:flowTable) : flowTable =
  List.rev
    (List.fold_left
       (fun acc flow ->
         if List.exists (fun flow' -> pat_subseteq flow.pattern flow'.pattern) acc then acc
         else flow::acc)
       [] t)

let rec inter f (t1:flowTable) (t2:flowTable) =
  List.fold_right
    (fun flow1 acc ->
      List.fold_right
        (fun flow2 acc ->
          match pat_inter flow1.pattern flow2.pattern with
            | Some pat ->
              mk_flow pat (f flow1.action flow2.action)::acc
            | None ->
              acc)
        t2 acc)
    t1 []

(* Union of t1 & t2 is their intersection followed by each of them, so  all
   cases are captured *)
let rec union f (t1:flowTable) (t2:flowTable) : flowTable =
  inter f t1 t2 @ t1 @ t2

let rec negate f t =
  List.map (fun flow -> { flow with action = f flow.action }) t

(* CPS compiler *)
let compile (pred : pred) (acts : action list) : flowTable =
  let rec aux pred k =
    match pred with
    | Test(NK.Switch(_)) ->
      k [ mk_flow all_pattern [[acts]] ]
    | Test(NK.Location(NK.Physical n)) -> 
      k [ mk_flow { all_pattern with inPort = Some n } [[acts]] ]
    | Test(NK.EthSrc n) ->
      k [ mk_flow { all_pattern with dlSrc = Some n } [[acts]] ]
    | Test(NK.EthDst n) ->
      k [ mk_flow { all_pattern with dlDst = Some n } [[acts]] ]
    | Test(NK.Vlan n) ->
      k [ mk_flow { all_pattern with dlVlan = Some n } [[acts]] ]
    | Test(NK.VlanPcp n) ->
      k [ mk_flow { all_pattern with dlVlanPcp = Some n } [[acts]] ]
    | Test(NK.EthType n) ->
      k [ mk_flow { all_pattern with dlTyp = Some n } [[acts]] ]
    | Test(NK.IPProto n) ->
      k [ mk_flow { all_pattern with nwProto = Some n } [[acts]] ]
    | Test(NK.IP4Src (n,_)) ->
      k [ mk_flow { all_pattern with nwSrc = Some (n, 0x20l) } [[acts]] ]
    | Test(NK.IP4Dst (n,_)) ->
      k [ mk_flow { all_pattern with nwDst = Some (n, 0x20l) } [[acts]] ]
    | Test(NK.TCPSrcPort n) ->
      k [ mk_flow { all_pattern with tpSrc = Some n } [[acts]] ]
    | Test(NK.TCPDstPort n) ->
      k [ mk_flow { all_pattern with tpDst = Some n } [[acts]] ]
    | Test _ -> 
      failwith "unsupported test"
    | Or(pred1,pred2) ->
      let f x y = x in
      aux pred1 (fun t1 ->
	aux pred2 (fun t2 ->
          k (optimize (union f t1 t2))))
    | And(pred1,pred2) ->
      let f x y = x in
      aux pred1 (fun t1 ->
        aux pred2 (fun t2 ->
          k (optimize (inter f t1 t2))))
    | Not(pred) ->
      let f x = if x = [] then  [[acts]] else [] in
      aux pred (fun t ->
        k (negate f t))
    | Everything ->
      k [ mk_flow all_pattern [[acts]] ]
    | Nothing ->
      k [ mk_flow all_pattern [] ] in
  aux pred (fun x -> optimize x)
    
let compile_flowtable (sw:Int64.t) (ofls: (pred * action list) list)
    : flowTable =
  List.fold_left
    (fun acc (pat,acts) ->
      let t = compile pat acts in
      optimize (union group_union t acc))
    [ mk_flow all_pattern [[]] ]
    ofls
