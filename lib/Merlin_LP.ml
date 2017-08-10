(* open Graph *)
open Frenetic_Network
open Merlin_NFA
open Merlin_Types
open Merlin_Gurobi
open Merlin_Globals
open Merlin_LPTypes
open Merlin_LPPretty
open Merlin_Dictionaries
open Merlin_BigGraph
open Merlin_Pretty

module type LP_HEURISTIC = sig
  val mk_objective : topo -> (BigGraph.t * NFA.t) list -> objective
  val mk_constraints : (BigGraph.t * NFA.t) list -> topo -> constraints
  val mk_bounds : topo -> bound list
  val mk_types : topo -> (BigGraph.t * NFA.t) list -> type_decl list
end



module type LP_SOLVER = sig
  val mk_graph : statement list -> topo -> int64 StringMap.t ->
    (Net.Topology.edge * NFA.edge * string) StringMap.t *  (BigGraph.t * NFA.t) list
  val solve : (BigGraph.t * NFA.t) list -> topo -> string list
  val collect : topo -> string list -> (Net.Topology.edge * NFA.edge * string) StringMap.t ->
    int64 StringMap.t -> int64 StringMap.t ->
    statement StringMap.t -> flow list

end

let hash_edges graphs_nfas =
  let h = PhysEdgeHash.create 101 in
  List.iter (fun (graph,_) ->
    BigGraph.iter_edges_e (fun e ->
      let src = Vertex.phys (Edge.src e) in
      let dst = Vertex.phys (Edge.dst e) in
      if (PhysEdgeHash.mem h (src,dst)) then
        PhysEdgeHash.replace h (src,dst) (e::(PhysEdgeHash.find h (src,dst)))
      else
        PhysEdgeHash.add h (src,dst) [e])
      graph) graphs_nfas;
  h

let rate_term t e =
  let n = Int64.to_float (edge_rate e) in
  Mult(Float(n), Var((edge_to_string t e)))

let utilization t graphs_nfas =
  let edges = hash_edges graphs_nfas in
  PhysEdgeHash.fold
    (fun (k1,k2) es acc ->
      let equation =
        List.fold_left
          (fun acc' e ->
            if acc' = Int(0L) then rate_term t e
            else Plus((rate_term t e),acc'))
          (Int(0L)) es in
      let usage = Printf.sprintf "%s_%s_bw_usage"
        (vertex_to_string t k1) (vertex_to_string t k2) in
      let expr = Minus(equation, Var(usage)) in
      let c = Constraint(expr,Eq,Int(0L)) in
      c::acc)
    edges []

let flow_conservation t graphs_nfas =
  List.fold_left
    (fun acc (graph,nfa) ->
      BigGraph.fold_vertex
        (fun v acc ->
          let ins =
            List.fold_left
              (fun acc e ->
                if acc = Int(0L) then rate_term t e
                else Plus(rate_term t e, acc))
              (Int(0L)) (BigGraph.pred_e graph v) in
          let outs =
            List.fold_left
              (fun acc e -> Minus(acc, rate_term t e))
              (Int(0L)) (BigGraph.succ_e graph v) in
          let state = Vertex.nfa v in
          let value =
            if NFA.state_init nfa state then
              (Int64.neg (edge_rate (List.hd (BigGraph.succ_e graph v))))
            else if NFA.state_accept_eps nfa state then
              (edge_rate (List.hd (BigGraph.pred_e graph v)))
            else
              0L in
          let conserv = Minus(ins, outs) in
          let expr = Constraint(conserv,Eq,Int(value)) in
          expr::acc)
        graph acc)
    [] graphs_nfas



module Make(H:LP_HEURISTIC) : LP_SOLVER = struct

  let mk_graph (statements:statement list) (phys:topo) (rate_tbl:(int64 StringMap.t)) =
    let phys_edges = Net.Topology.edges phys in
    let mk_big_edge stmt_var phys_edge phys_src phys_dst nfa_edge rate generate =
      let (nfa_src,nfa_dst) = Merlin_NFA.NFA.edge_src_dst nfa_edge in
      let src = (nfa_src, phys_src) in
      let dst = (nfa_dst, phys_dst) in
      let elabel = Net.Topology.edge_to_label phys phys_edge in
      let l = {rate = rate;
               capacity = Link.capacity elabel;
               cost=Link.cost elabel;
               stmt_var=stmt_var;
               generatable=generate;
              } in
      let l' = {Edge.xxx=fresh_id ()} in
      let e = (src, l', dst) in
      EdgeHash.add edge_to_label e l;
      e
    in
    (* TODO(jnf): think about how to optimize this. *)
    let big_graph nfa rate stmt_var m =
      let graph = BigGraph.create ~size:10000 () in
      let m = Net.Topology.EdgeSet.fold phys_edges ~init:m ~f:(fun m phys_edge ->
            let phys_src,_ = (Net.Topology.edge_src phys_edge) in
            let phys_dst,_ = (Net.Topology.edge_dst phys_edge) in
            let phys_src_str = vertex_to_string phys phys_src in
            let phys_dst_str = vertex_to_string phys phys_dst in
            let m' = Merlin_NFA.NFA.EdgeSet.fold
              (fun (nfa_edge:Merlin_NFA.NFA.edge) m ->
                let _,m' = NFA.CharSet.fold (fun c ((all_done,m) as acc) ->
                  if all_done then acc
                  else
                    try
                      let s = SymbolHash.find symbol_to_location c in
                      (* TODO(rjs): we need a better solution than string comparisons *)
                      if s = phys_src_str then
                        let generate =
                          if ("_in_" = phys_src_str) then
                            false
                          else if ("_out_" = phys_dst_str) then
                            false
                          else
                            true in
                        let big_edge = mk_big_edge stmt_var phys_edge phys_src phys_dst nfa_edge rate generate in
                        let name = edge_to_string phys big_edge in
                        StringHash.add name_to_edge name big_edge;
                        BigGraph.add_edge_e graph big_edge;
                        (true,
                         StringMap.add name (phys_edge,nfa_edge,stmt_var) m)
                      else
                        acc
                    with Not_found ->
                      acc)
                  (Merlin_NFA.NFA.edge_symbols nfa_edge) (false,m) in
                m')
              (NFA.edges nfa) m in
            m') in
      (graph,m)
    in
    let bg_start = Merlin_Time.time () in
    let m,graphs_nfas = List.fold_left (fun (m,graphs_nfas) s ->
      let Statement(v,p,e) = s in
      let min =
        try StringMap.find v rate_tbl
        with Not_found -> begin Printf.printf "*** Not found v is %s \n" v; 0L end
      in
      let nfa = NFA.of_regex e in
      let graph,m' = big_graph nfa min v m in
      (m', (graph,nfa)::graphs_nfas)) (StringMap.empty,[]) statements in
    let bg_stop = Merlin_Time.from bg_start in
    Merlin_Stats.graph_construct := Merlin_Time.to_nsecs bg_stop;
    if (!verbose) then Printf.printf "Graph construction time:\t%f%!\n" !Merlin_Stats.graph_construct;
    m,graphs_nfas

  let solve (graphs_nfas:(BigGraph.t * NFA.t) list) (phys:topo)  =
    let start = Merlin_Time.time () in

    let obj_start = Merlin_Time.time () in
    let obj = H.mk_objective phys graphs_nfas in
    let obj_stop = Merlin_Time.from obj_start in
    Merlin_Stats.obj_construct := Merlin_Time.to_nsecs obj_stop;
    if (!verbose) then Printf.printf "Objective construction time:\t%f%!\n" !Merlin_Stats.obj_construct;

    let const_start = Merlin_Time.time () in
    let const = H.mk_constraints graphs_nfas phys in
    let const_stop = Merlin_Time.from const_start in
    Merlin_Stats.const_construct := Merlin_Time.to_nsecs const_stop;
    if (!verbose) then Printf.printf "Const construction time:\t%f%!\n" !Merlin_Stats.const_construct;

    let bounds_start = Merlin_Time.time () in
    let bounds = H.mk_bounds phys in
    let bounds_stop = Merlin_Time.from bounds_start in
    Merlin_Stats.bounds_construct :=  Merlin_Time.to_nsecs bounds_stop;
    if (!verbose) then Printf.printf "Bounds construction time:\t%f%!\n" !Merlin_Stats.bounds_construct;

    let types = H.mk_types phys graphs_nfas in

    let sos = NoSos in

    let lp = LP(obj, const, bounds, types, sos) in
    let stop = Merlin_Time.from start in
    Merlin_Stats.lp_construct := Merlin_Time.to_nsecs stop;
    if (!verbose) then Printf.printf "LP construction time:\t%f%!\n" !Merlin_Stats.lp_construct;
    let size = List.fold_left
      (fun acc (graph,_) ->
        (BigGraph.nb_edges graph) + acc
      )
      0 graphs_nfas in
    Merlin_Stats.var_count := size;
    let solution = Merlin_Gurobi.call lp in
    if (!verbose) then Printf.printf "Variables:\t%d\n" !Merlin_Stats.var_count;
    solution

  let collect = Merlin_Gurobi.solution_to_flows
end

module ShortestPathHeuristic : LP_HEURISTIC  = struct
  let mk_objective phys graphs_nfas =
    let terms = List.fold_left (fun acc (g,_) ->
      BigGraph.fold_edges_e (fun e acc ->
        let n = Int64.to_float (Int64.mul (edge_rate e) (edge_cost e)) in
        let term = Mult(Float(n), Var((edge_to_string phys e))) in
        if acc = Int(0L) then term else Plus(term,acc)
      ) g acc
    ) (Int(0L)) graphs_nfas in
    Minimize([terms])

  let mk_constraints graphs_nfas phys =
    (utilization phys graphs_nfas)@(flow_conservation phys graphs_nfas)

  let mk_bounds phys =
    Net.Topology.EdgeSet.fold (Net.Topology.edges phys) ~init:[]
      ~f:(fun acc phys_edge ->
        let label = Net.Topology.edge_to_label phys phys_edge in
        let src,_ = (Net.Topology.edge_src phys_edge) in
        let dst,_ = (Net.Topology.edge_dst phys_edge) in
        let usage = Printf.sprintf "%s_%s_bw_usage"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let b = Bound(Var(usage),Leq,Int(Link.capacity label)) in
        b::acc)

  let mk_types phys graphs_nfas  =
    let terms = List.fold_left
      (fun acc (graph,_) ->
        BigGraph.fold_edges_e
          (fun e acc -> (edge_to_string phys e)::acc) graph acc)
      [] graphs_nfas in
    [Binary(terms)]

end

module MinMaxRatioHeuristic : LP_HEURISTIC  = struct
  let mk_objective phys graphs_nfas = Minimize([Var("max_load_ratio")])

  let mk_constraints graphs_nfas phys =
    let slack_constraints phys =
      let phys_edgs = Net.Topology.edges phys in
      Net.Topology.EdgeSet.fold phys_edgs ~init:[] ~f:(fun acc phys_edge ->
        let label = Net.Topology.edge_to_label phys phys_edge in
        let src,_ = Net.Topology.edge_src phys_edge in
        let dst,_ = Net.Topology.edge_dst phys_edge in
        let capacity = Link.capacity label in
        let usage = Printf.sprintf "%s_%s_bw_usage"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let slack = Printf.sprintf "%s_%s_slack"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let mult = Mult ( Int(capacity), Var("max_load_ratio") ) in
        let expr = Minus( mult, Minus(Var(usage), Var(slack))) in
        Constraint(expr,Eq,Int(0L))::acc )
    in
    let u = utilization phys graphs_nfas in
    let f = flow_conservation phys graphs_nfas in
    let s = slack_constraints phys in
    u@f@s

  let mk_bounds phys =
    Net.Topology.EdgeSet.fold (Net.Topology.edges phys) ~init:[]
      ~f:(fun acc phys_edge ->
        let src,_ = Net.Topology.edge_src phys_edge in
        let dst,_ = Net.Topology.edge_dst phys_edge in
        let label = Net.Topology.edge_to_label phys phys_edge in
        let usage = Printf.sprintf "%s_%s_bw_usage"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let b = Bound(Var(usage),Leq,Int(Link.capacity label)) in
        let slack = Printf.sprintf "%s_%s_slack"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let c = Bound( Var(slack),Geq,Int(0L)) in
        c::b::acc)

  let mk_types phys graphs_nfas  =
    let terms = List.fold_left
      (fun acc (graph,_) ->
        BigGraph.fold_edges_e
          (fun e acc -> (edge_to_string phys e)::acc) graph acc)
      [] graphs_nfas in
    [Binary(terms)]
end

module MinMaxReservedHeuristic : LP_HEURISTIC  = struct
  let mk_objective phys graphs_nfas = Minimize([Var("max_load")])

  let mk_constraints graphs_nfas phys =
    let slack_constraints phys =
      let phys_edgs = Net.Topology.edges phys in
      Net.Topology.EdgeSet.fold phys_edgs ~init:[] ~f:(fun acc phys_edge ->
        let src,_ = Net.Topology.edge_src phys_edge in
        let dst,_ = Net.Topology.edge_dst phys_edge in
        let usage = Printf.sprintf "%s_%s_bw_usage"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let slack = Printf.sprintf "%s_%s_slack"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let expr = Minus( Var("max_load"), Minus(Var(usage), Var(slack))) in
        Constraint(expr,Eq,Int(0L))::acc )
    in
    let u = utilization phys graphs_nfas in
    let f = flow_conservation phys graphs_nfas in
    let s = slack_constraints phys in
    u@f@s

  let mk_bounds phys =
    Net.Topology.EdgeSet.fold (Net.Topology.edges phys) ~init:[]
      ~f:(fun acc phys_edge ->
        let src,_ = Net.Topology.edge_src phys_edge in
        let dst,_ = Net.Topology.edge_dst phys_edge in
        let label = Net.Topology.edge_to_label phys phys_edge in
        let usage = Printf.sprintf "%s_%s_bw_usage"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let b = Bound(Var(usage),Leq,Int(Link.capacity label)) in
        let slack = Printf.sprintf "%s_%s_slack"
          (vertex_to_string phys src)
          (vertex_to_string phys dst) in
        let c = Bound( Var(slack),Geq,Int(0L)) in
        c::b::acc)

  let mk_types phys graphs_nfas  =
    let terms = List.fold_left
      (fun acc (graph,_) ->
        BigGraph.fold_edges_e
          (fun e acc -> (edge_to_string phys e)::acc) graph acc)
      [] graphs_nfas in
    [Binary(terms)]
end
