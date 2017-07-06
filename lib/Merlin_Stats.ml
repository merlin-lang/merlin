open Frenetic_Network
open Merlin_Globals
open Merlin_Topology

(* Timings *)

let lp_write = ref 0.0
let gurobi_soln = ref 0.0
let soln_read = ref 0.0
let graph_construct = ref 0.0
let obj_construct = ref 0.0
let bounds_construct = ref 0.0
let lp_construct = ref 0.0
let const_construct = ref 0.0
let rless_pathgen = ref 0.0
let stepgen = ref 0.0
let of_compile = ref 0.0

(* Fine-grained timing and stats for sink trees *)
let sink_cross = ref 0.0
let sink_trees = ref 0.0
let sink_fwdnet = ref 0.0
let sink_irgen = ref 0.0
let cross_states = ref 0
let cross_edges = ref 0
let ave_bfs = ref 0.0
let bfs_inner_loop = ref 0

(* Output and IR counts *)
let tree_count = ref 0
let sp_count = ref 0L
let stmts_count = ref 0
let rated_count = ref 0
let rateless_count = ref 0
let qc_count = ref 0
let step_count = ref 0
let of_count = ref 0
let tc_count = ref 0
let var_count = ref 0

let print_stats topo =
  if (!stat) then begin
    (* Topology information *)
    Printf.printf "Switches:\t%d\n" (Net.Topology.VertexSet.length (switches topo));
    Printf.printf "Hosts:\t%d\n" (Net.Topology.VertexSet.length (hosts topo));
    Printf.printf "Unidirectional Links:\t%d\n"
      (Net.Topology.EdgeSet.length (Net.Topology.edges topo));

    (* Policy information *)
    Printf.printf "Statements:\t%d\n" !stmts_count;
    Printf.printf "Rated Statements:\t%d\n" !rated_count;
    Printf.printf "Rateless Statements:\t%d\n" !rateless_count;
    Printf.printf "Cross NFA States:\t%d\n" !cross_states;
    Printf.printf "Cross NFA Edges:\t%d\n" !cross_edges;
    Printf.printf "Average BFS inner loop:\t%d\n" !bfs_inner_loop;
    Printf.printf "Trees:\t%d\n" !tree_count;

    (* Output information *)
    Printf.printf "Steps rules:\t%d\n" !step_count;
    Printf.printf "OpenFlow rules:\t%d\n" !of_count;
    Printf.printf "Queue Configurations:\t%d\n" !qc_count;
    Printf.printf "tc Configurations:\t%d\n" !tc_count;
    Printf.printf "Variables:\t%d\n" !var_count;
    Printf.printf "Shortest path called:\t%Ld\n" ! sp_count;

    if (!timing) then begin
      Printf.printf "Graph construction time:\t%f\n" !graph_construct;
      Printf.printf "Graph construction time: %f (sec)\n" (!graph_construct);
      Printf.printf "Objective construction time:\t%f\n" !obj_construct;
      Printf.printf "Const construction time:\t%f\n" !const_construct;
      Printf.printf "Bounds construction time:\t%f\n" !bounds_construct;
      Printf.printf "LP construction time:\t%f\n" !lp_construct;
      Printf.printf "LP write time:\t%f\n" !lp_write;
      Printf.printf "Gurobi time:\t%f\n" !gurobi_soln;
      Printf.printf "Rateless solution time:\t%f\n" !rless_pathgen;
      Printf.printf "Rateless step generation time:\t%f\n" !stepgen;
      Printf.printf "OpenFlow compilation time:\t%f\n" !of_compile;
      Printf.printf "Cross product time:\t%f\n" !sink_cross;
      Printf.printf "Sink tree generation time:\t%f\n" !sink_trees;
      Printf.printf "Forwarding net time:\t%f\n" !sink_fwdnet;
      Printf.printf "IR Generation time:\t%f\n" !sink_irgen;
      Printf.printf "Average BFS time:\t%f\n" !ave_bfs
    end;

  end
