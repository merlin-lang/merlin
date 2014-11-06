open Graph
open Network_Common
open Merlin_NFA
open Merlin_Types
open Merlin_Pretty
open Merlin_Globals
open Merlin_Dictionaries

module Vertex = struct
  type t = (NFA.state * vertex)
  let equal = Pervasives.(=)
  let hash = Hashtbl.hash
  let phys (_,p) = p
  let nfa (n,_) = n
  let compare = Pervasives.compare
end

module PhysEdgeOrd = struct
  type t = vertex
  let compare = Pervasives.compare
end

module PhysNodeHash = Hashtbl.Make(struct
  type t = vertex
  let hash = Hashtbl.hash
  let equal = (=)
end)

module PhysEdgeHash = Hashtbl.Make(struct
  type t = PhysEdgeOrd.t * PhysEdgeOrd.t
  let hash = Hashtbl.hash
  let equal = (=)
end)

module VariableHash = Hashtbl.Make(struct
  type t = vertex * vertex * NFA.state * NFA.state * string
  let hash = Hashtbl.hash
  let equal = (=)
end)

let edge_variables : string VariableHash.t = VariableHash.create 17

let var_num = ref 0

let fresh_var () : int =
  incr var_num ; !var_num

let reset_var_num () =
  var_num := 0

let edge_id = ref 0

let fresh_id () : int =
  incr edge_id ; !edge_id

let reset_edge_id () =
  edge_id := 0


let get_variable phy_src phy_dst nfa_src nfa_dst stmt_var =
  let t = (phy_src, phy_dst, nfa_src,nfa_dst, stmt_var) in
  if VariableHash.mem edge_variables t then
    VariableHash.find edge_variables t
  else
    begin
      let n = fresh_var () in
      let v = Printf.sprintf "v%d" n in
      VariableHash.add edge_variables t v;
      v
    end

type edge_label = {rate : Int64.t; capacity : Int64.t; cost : Int64.t; stmt_var:string; generatable:bool; }

module Edge = struct
  type v = Vertex.t
  type t = {xxx : int;}
  type e = v * t * v
  let default =  {xxx=0;}
  let mk_edge s l d = (s,l,d)
  let src (s,_,_) = s
  let dst (_,_,d) = d
  let compare = Pervasives.compare
end

module EdgeHash = Hashtbl.Make(struct
  type t = Edge.e
  let hash = Hashtbl.hash
  let equal = (=)
end)

let edge_to_label : edge_label EdgeHash.t = EdgeHash.create 1000

let edge_cost e = let l = EdgeHash.find edge_to_label e in l.cost
let edge_rate e = let l = EdgeHash.find edge_to_label e in l.rate
let edge_stmt_var e = let l = EdgeHash.find edge_to_label e in l.stmt_var
let edge_generatable e = let l = EdgeHash.find edge_to_label e in l.generatable

let edge_to_string t e =
  let (s,_,d) = e in
  let nfa_src,phy_src = s in
  let nfa_dst,phy_dst = d in
  let src_label = Net.Topology.vertex_to_label t phy_src in
  let dst_label = Net.Topology.vertex_to_label t phy_dst in
  if (not !shortname) then
    Printf.sprintf "v_%s_%s_%s_%s_%s"
      (Node.name src_label)
      (Node.name dst_label)
      (NFA.state_name nfa_src)
      (NFA.state_name nfa_dst)
      (edge_stmt_var e)
  else
    get_variable phy_src phy_dst nfa_src nfa_dst (edge_stmt_var e)

let name_to_edge : Edge.e StringHash.t = StringHash.create 1000

module BigGraph = struct
  include Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)
end

module CrossGraph = struct

  module Weight = struct
    type t = int
    type label = unit
    let weight l = 1
    let compare = Pervasives.compare
    let add = (+)
    let zero = 0
  end

  module I = Imperative.Digraph.ConcreteBidirectional(Vertex)
  include I
  module Dij = Path.Dijkstra(I)(Weight)

  module Elt = struct
    type t = Weight.t * I.V.t

  (* weights are compared first, and minimal weights come first in the
     queue *)
    let compare (w1,v1) (w2,v2) =
      let cw = Weight.compare w2 w1 in
      if cw != 0 then cw else I.V.compare v1 v2
  end
  module Q = Heap.Imperative(Elt)
  module H = Hashtbl.Make(I.V)

  let cross (r:regex) (t:topo) : (t * NFA.state * NFA.state) =
    let nfa = NFA.of_regex r in
    let links = Net.Topology.edges t in
    let transitions = NFA.edges nfa in
    let graph = create
      ~size:((Net.Topology.EdgeSet.cardinal links) * (NFA.EdgeSet.cardinal transitions)) () in
    Net.Topology.EdgeSet.iter (fun pe ->
      let p_src,_ = Net.Topology.edge_src pe in
      let p_dst,_ = Net.Topology.edge_dst pe in
      let src_str = Node.name (Net.Topology.vertex_to_label t p_src) in
      NFA.EdgeSet.iter (fun t ->
        NFA.CharSet.iter (fun c ->
          let (s, _) = SymbolHash.find symbol_to_code c in
          if s = src_str then
            let n_src,n_dst = NFA.edge_src_dst t in
            add_edge graph (n_src,p_src) (n_dst,p_dst)
          else ()
        ) (NFA.edge_symbols t)
      ) transitions
    ) links;
    let srcstate = NFA.StateSet.choose (NFA.inits nfa) in
    let dststate = NFA.StateSet.choose (NFA.accept_eps nfa) in
    (graph,srcstate,dststate)

  let shortest_path (g:t) (src:Vertex.t) (dst:Vertex.t) : E.t list =
	let ret,_ = Dij.shortest_path g src dst in ret

  let spanning_tree_from (g:t) (n:V.t) =
    let visited = H.create 97 in
    let key = H.create 97 in
    let q = Q.create 17 in
    Q.add q (Weight.zero, n);
    while not (Q.is_empty q) do
      let (w,u) = Q.pop_maximum q in
      if not (H.mem visited u) then begin
	    H.add visited u ();
	    I.iter_succ_e (fun e ->
	      let v = I.E.dst e in
	      if not (H.mem visited v) then begin
	        let wuv = Weight.weight (I.E.label e) in
	        let improvement =
              try Weight.compare wuv (fst (H.find key v)) < 0
              with Not_found -> true
            in
            if improvement then begin
              H.replace key v (wuv, e);
              Q.add q (wuv, v)
            end;
	      end) g u
      end
    done;
    H.fold (fun _ (_, e) acc -> e :: acc) key []

  let project_physical (es:I.edge list) : topo =
    Net.Topology.empty ()
end
