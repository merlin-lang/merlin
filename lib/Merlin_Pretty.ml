open Format
open Frenetic_Packet
open Pervasives
open Frenetic_Network
open Merlin_Types
open Merlin_Util
open Merlin_NFA
open Merlin_Dictionaries


(* OpenFlow-related pretty printers, exported from SDN_Types *)
module Pred = struct

  let rec pred fmt p = match p with
    | And (p1, p2) -> fprintf fmt "@[(%a@ and %a)@]" orpred p1 pred p2
    | _ -> orpred fmt p

  and orpred fmt p = match p with
    | Or (p1, p2) -> fprintf fmt "@[(%a@ or %a)@]" apred p1 pred p2
    | _ -> apred fmt p

  and apred fmt p = 
    let open Frenetic_NetKAT in 
    match p with
    | Test(Switch n) -> fprintf fmt "@[switch@ =@ %Ld@]" n
    | Test(Location(Physical n)) -> fprintf fmt "@[port@ =@ %ld@]" n
    | Test(EthSrc n) -> fprintf fmt "@[ethSrc@ =@ %Ld@]" n
    | Test(EthDst n) -> fprintf fmt "@[ethDst@ =@ %Ld@]" n
    | Test(Vlan n) -> fprintf fmt "@[vlanId@ =@ %d@]" n
    | Test(VlanPcp n) -> fprintf fmt "@[vlanPcp@ =@ %d@]" n
    | Test(EthType n) -> fprintf fmt "@[ethTyp@ =@ %d@]" n
    | Test(IPProto n) -> fprintf fmt "@[nwProto@ =@ %d@]" n
    | Test(IP4Src(n,_)) -> fprintf fmt "@[ipSrc@ =@ %ld@]" n
    | Test(IP4Dst(n,_)) -> fprintf fmt "@[ipDst@ =@ %ld@]" n
    | Test(TCPSrcPort(n)) -> fprintf fmt "@[tcpSrcPort@ =@ %d@]" n
    | Test(TCPDstPort(n)) -> fprintf fmt "@[tcpDstPort@ =@ %d@]" n
    | Test _ -> failwith "unsupported field"
    | Not p' -> fprintf fmt "@[!(%a)@]" apred p'
    | Everything -> fprintf fmt "@[*@]"
    | Nothing -> fprintf fmt "@[none@]"
    | Or _
    | And _ -> fprintf fmt "@[(%a)@]" pred p

  let to_string p =
    let buf = Buffer.create 100 in
    let fmt = formatter_of_buffer buf in
    pp_set_margin fmt 80;
    pred fmt p;
    fprintf fmt "@?";
    Buffer.contents buf

end

let string_of_pred = Pred.to_string
let string_of_action = SDN_Types.string_of_action
let string_of_flowTable = SDN_Types.string_of_flowTable

(* End OpenFlow-related printers *)

let string_of_info f ((l1,c1),(l2,c2)) =
  if l2=l1 then
    Printf.sprintf "File \"%s\", line %d, characters %d-%d" f l1 c1 c2
  else
    Printf.sprintf "File \"%s\", line %d, character %d, to line %d, character %d" f l1 c1 l2 c2

(* Pretty Printers for Merlin datatypes *)

let rec string_of_regex r =

  (* TODO(rjs): this is a hack. If we have an alt with a bunch of function that maps to
     a set of host, we only want to print the function once. In other words:
     h1 | h2 should print fire1, not fire1 | fire1.
  *)
  let string_of_alt e =
    match e with
    | Alt(Char(n), Char(m)) ->
      let open Merlin_Dictionaries in
      begin
        (* TODO(basus): Check if the locations match some abstract network function *)
        let n' = SymbolHash.find symbol_to_location n in
        let m' = SymbolHash.find symbol_to_location m in
        Printf.sprintf "(%s | %s" n' m'
     end
    | Alt(r1,r2) -> Printf.sprintf "( %s | %s )" ( string_of_regex r1 ) (
        string_of_regex r2 )
    | _ -> assert (false)
  in

  match r with
  | AnyChar ->
    "."
  | Char(n) ->
    ( try Merlin_Dictionaries.SymbolHash.find
            Merlin_Dictionaries.symbol_to_location n
      with Not_found ->
        failwith (Printf.sprintf "Malformed regex: %d not found in symbol_to_code" n))
  | Alt(lhs, rhs) ->
    begin
      match r with
      | Alt(Char(n), Char(m)) -> string_of_alt r
      | Alt(lhs, rhs) ->  Printf.sprintf("%s | %s") (string_of_regex lhs) (string_of_regex rhs)
      | _ -> assert (false)
    end

  | Cat(lhs, rhs) ->
    Printf.sprintf("%s %s") (string_of_regex lhs) (string_of_regex rhs)
  | Kleene(expr) ->
    Printf.sprintf("%s*") (string_of_regex expr)
  | Group(expr) ->
    Printf.sprintf("( %s )") (string_of_regex expr)
  | Empty -> ""

let string_of_operator (op:operator) = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let string_of_locationset (ls:LocationSet.t) =
    Printf.sprintf "{%s}" (LocationSet.intercalate (fun x -> x) ";" ls)

let string_of_pair (p:pair) =
  let Pair(l1,l2) = p in
  Printf.sprintf "(%s,%s)" l1 l2


let rec string_of_expr (e:expr) = match e with
  | Binary(op,e1,e2) -> Printf.sprintf "(%s %s %s)"
                        (string_of_expr e1)
                        (string_of_operator op)
                        (string_of_expr e2)
  | RateLiteral(i) -> Int64.to_string i

let string_of_expansion (e:expansion) = match e with
  | Expansion (s,ls1,ls2) -> Printf.sprintf "%s(%s,%s)"
                        s
                        (string_of_locationset ls1)
                        (string_of_locationset ls2)

let string_of_comprehension (c:comprehension) =
  let Foreach(p,e) = c in
  Printf.sprintf "foreach %s : %s"
    (string_of_pair p)
    (string_of_expansion e)

let string_of_rate r =
  let Rate(min,max) = r in
  Printf.sprintf "min(%Ld bps) max(%Ld bps)" min max

let string_of_rate_option r =
  match r with
  | RMin(n) -> Printf.sprintf "min(%Ld bps)" n
  | RMax(n) -> Printf.sprintf "max(%Ld bps)" n
  | RBoth(n,m) -> Printf.sprintf "min(%Ld bps) max(%Ld bps)" n m
  | RNone -> ""

let string_of_policy_ast s =
  let ASTPolicy(c,p,a,r) = s in
  Printf.sprintf("%s\n%s -> %s at %s;\n")
    (string_of_comprehension c)
    (string_of_pred p)
    (string_of_regex a)
    (string_of_rate_option r)

let string_of_program_ast p =
  let ASTProgram(l) = p in list_intercalate string_of_policy_ast "\n" l

let string_of_swqconf qc =
  let QConf(sw,p,q,min,max) = qc in
  Printf.sprintf "Switch:%Ld port:%ld queue:%ld rate:(%Ld, %Ld)"
    sw p q min max

let vertex_to_string (t:Net.Topology.t) (n:vertex) =
  let label = Net.Topology.vertex_to_label t n in
  Node.name label

let string_of_vertex (t:Net.Topology.t) (n:vertex) =
  let label = Net.Topology.vertex_to_label t n in
  Node.name label

let string_of_opf opf =
  let (pr,act,sw) = opf in
  Printf.sprintf "On switch: %Ld \nMatching: %s \nDo: %s\n"
    sw (string_of_pred pr) (SDN_Types.string_of_action act)

let string_of_hop (h:hop) = match h with
  | Ingress -> "Ingress"
  | Egress -> "Egress"
  | Intermediate -> "Intermediate"
  | IngressEgress -> "IngressEgress"
  | Nohop -> "Nohop"

let string_of_instruction instr =
  match instr with
  | Forward(pt,sw,pt',r,h) ->
    let rs = match r with
      | Some (min,max) -> Printf.sprintf "(%Ld,%Ld)" min max
      | None -> "None" in
    Printf.sprintf "Forward(%ld,%Ld,%ld,%s,%s)"
      pt
      sw
      pt'
      rs
      (string_of_hop h)
  | Throttle(h,(min,max)) ->
     Printf.sprintf "Throttle(%s,(%Ld,%Ld))" (string_of_ip h) min max
  | Function(h,f) ->
     Printf.sprintf "Function(%s,%s)" h (list_intercalate (fun x -> x) "," f)

let string_of_instructions l =
  Printf.sprintf "<\n%s\n>\n"
    (list_intercalate string_of_instruction "\n" l)

let string_of_forward t f =
  let of_hop h = match h with
      | Ingress -> "i"
      | Intermediate -> "t"
      | Egress -> "e"
      | IngressEgress -> "ie"
      | Nohop -> "n" in
  let open Printf in
  let label = Net.Topology.vertex_to_label t f.topo_vertex in
  let v = Net.Topology.Vertex.to_string label in
  let kind = match Node.device label with
    | Node.Host -> "host"
    | Node.Switch -> "switch"
    | Node.Middlebox -> "mbox" in
  let inp = match f.in_port with None -> "" | Some p -> sprintf "%ld" p in
  let outp = match f.out_port with None -> "" | Some p -> sprintf "%ld" p in
  let inh = of_hop f.in_hop in
  let outh = of_hop f.out_hop in
  sprintf "%s%s:(%s %s):%s%s" inp inh kind v outp outh

let string_of_flow t f =
  let buf = Buffer.create 100 in
  Buffer.add_string buf (string_of_pred (fst f));
  Buffer.add_string buf " -> [ ";

  List.iter
    (fun f ->
       Buffer.add_string buf " ";
       Buffer.add_string buf (string_of_forward t f);
       Buffer.add_string buf "; " )
    (snd f) ;
  Buffer.add_string buf "]\n";
  Buffer.contents buf


(* Print Presurger reformulation types *)

let rec string_of_bandwidth (b:bandwidth) = match b with
  | BLit(i) -> Printf.sprintf "%Ld bps" i
  | BVar(v) -> v
  | BSum(b1, b2) -> Printf.sprintf "(%s + %s)"
                    (string_of_bandwidth b1)
                    (string_of_bandwidth b2)

let rec string_of_formula (f:formula) = match f with
  | FMax(b,i) -> Printf.sprintf "max(%s, %Ld bps)" (string_of_bandwidth b) i
  | FMin(b,i) -> Printf.sprintf "min(%s, %Ld bps)" (string_of_bandwidth b) i
  | FAnd(f1, FNone) -> (string_of_formula f1)
  | FAnd(FNone, f2) -> (string_of_formula f2)
  | FAnd(f1, f2) -> Printf.sprintf "%s and %s"
                    (string_of_formula f1)
                    (string_of_formula f2)
  | FOr(f1, f2) -> Printf.sprintf "%s or %s"
                    (string_of_formula f1)
                    (string_of_formula f2)
  | FNeg(f) -> Printf.sprintf "(not %s)"
                    (string_of_formula f)
  | FNone -> ""


let string_of_statement (s:statement) =
  let Statement(p,r,v) = s in
  Printf.sprintf "[%s, %s, %s]" 
    (string_of_pred p) (string_of_regex r) v

let string_of_policy (p:policy) =
  let Policy(sl,f) = p in
  let slstring = intercalate string_of_statement "\n" sl in
  let fstring = string_of_formula f in
  slstring ^ "\n" ^ fstring

let rated_soln_to_string topo edges varmap =
  let buffer = Buffer.create (List.length edges) in
  Buffer.add_string buffer "digraph solution {\n";
  List.iter (fun edge ->
    let (e,_,i) = StringMap.find edge varmap in
    let src,_ = Net.Topology.edge_src e in
    let dst,_ = Net.Topology.edge_dst e in
    let edge_str = Printf.sprintf "%s -> %s [label=\"%s\"];\n"
        (vertex_to_string topo src) (vertex_to_string topo dst) i in
    Buffer.add_string buffer edge_str
  ) edges;
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let print_cross_tbl tbl =
  Printf.printf "Printing cross table\n";
  Hashtbl.iter (fun k v ->
    Printf.printf "%s -> [" (Merlin_NFA.NFA.state_name k);
    List.iter (fun s -> Printf.printf "%s " (Merlin_NFA.NFA.state_name s)) v;
    Printf.printf "]\n";
  ) tbl ;
  Printf.printf "\n"

module Stat = struct
  let string_of_ofs (ofs : (pred * action list * switchId) list) : string =
    let hdr = "Switch\tPredicate\tActions\n" in
    let body = intercalate (fun opf ->
      let (pred, actions, switch) = opf in
      let pr = (string_of_pred pred) in
      let acts = intercalate SDN_Types.string_of_action "," actions in
      Printf.sprintf "%Ld\t%s\t%s" switch pr acts
    ) "\n" ofs in
    hdr ^ body

  let string_of_qcs (qcs:swqconf list) : string =
    let hdr = "Switch\tPort\tQueue\tRate\t" in
    let body = intercalate (fun qc ->
      let QConf(sw,p,q,min, max) = qc in
      Printf.sprintf "%Ld\t%ld\t%ld\t(%Ld,%Ld)"
        sw p q min max
    ) "\n" qcs in
    hdr ^ body

end

module Test = struct
  open Node
  let string_of_flows (t:topo) (fs: flow list) : string =
    let acc =  (Buffer.create 1000) in
    List.iter (fun f ->
      let (_,forwards) = f in
      List.iter (fun fwd ->
        let node,devid = fwd.device in
        let label = Net.Topology.vertex_to_label t fwd.topo_vertex in
        let ip = Node.ip label in
        Buffer.add_string acc (Printf.sprintf "%s:%Ld " (string_of_ip ip) devid)
      ) (List.sort Pervasives.compare forwards) ;
      Buffer.add_string acc "\n"
    ) (List.sort Pervasives.compare fs) ;
    Buffer.contents acc

  let string_of_ofs (ofs: step list) : string = ""
 let string_of_qcs (qcs:swqconf list) : string = ""

 let string_of_tcs (tcs: (nwAddr * string) list) : string = ""
end
