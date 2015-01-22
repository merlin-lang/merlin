open Packet
open Network_Common

open Merlin_Types
open Merlin_Util
open Merlin_Error
open Merlin_Pretty
open Merlin_Globals
open Merlin_Topology
open Merlin_Dictionaries
open Merlin_BigGraph
open Merlin_Preprocess

let reset () =
  StringHash.reset symbol_table;
  StringHash.reset name_to_edge;
  reset_edge_id () ;
  reset_var_num ()

let parse_string target str =
  let lexbuf = Lexing.from_string str in
  target Merlin_Lexer.main lexbuf

let parse_file target file  =
  let lexbuf = Lexing.from_channel (open_in file) in
  target Merlin_Lexer.main lexbuf

let parse_regex_string str =
  parse_string Merlin_Parser.regex str

let parse_policy_string str =
  parse_string Merlin_Parser.program str

let parse_policy_file fname =
  reset ();
  parse_file Merlin_Parser.program fname

let parse_topo_file fname =
  try
    let open Net.Topology in
    let topo = Net.Parse.from_dotfile fname in
    let nodes = vertexes topo in
    let _ = VertexSet.iter nodes ~f:(fun n ->
      let label = vertex_to_label topo n in
      let name = Node.name label in
      let ip = Node.ip label in
      LocationHash.add location_to_node name n;
      LocationHash.add location_to_addr name ip;
      AddrHash.add addr_to_location ip name;
      let _ = get_symbol (name, None) in ()
    ) in
    topo
  with _ -> raise Topology_parse_error

let parse_ir_string str =
  parse_string Merlin_Parser.ir_policy str

let parse_ir_file fname =
  reset ();
  parse_file Merlin_Parser.ir_policy fname

let rec vars_to_rates formula ((mins, maxs) as acc) =
  match formula with
  | FMin(b, n) ->
    begin match b with
    | BVar(v) -> ((StringMap.add v n mins), maxs)
    | BSum(_,_) ->  raise Non_local_policy
    | _ -> raise Non_local_policy
    end
  | FMax(b, n) ->
    begin match b with
    | BVar(v) -> (mins, (StringMap.add v n maxs))
    | _ -> raise Non_local_policy
    end
  | FAnd(f1,f2) -> let acc' = (vars_to_rates f1 acc) in (vars_to_rates f2 acc')
  | FOr(f1,f2) ->  let acc' = (vars_to_rates f1 acc) in (vars_to_rates f2 acc')
  | FNeg(f) ->  (vars_to_rates f acc)
  | FNone -> acc

let partition (Policy(statements, formula)) =
  let var_to_stmt = List.fold_left
    (fun acc (Statement(_,_,id) as stmt) -> StringMap.add id stmt acc) StringMap.empty statements in
  let (mins, maxs) = vars_to_rates formula (StringMap.empty, StringMap.empty) in
  let (guarantees, others) = List.partition (fun (Statement(_,_,id)) -> StringMap.mem id mins) statements in
  Merlin_Stats.rated_count := (List.length guarantees);
  Merlin_Stats.rateless_count := (List.length others);
  (guarantees, others, mins, maxs, var_to_stmt)

let partition_ast (program:ast_program) : (ast_policy list * ast_policy list) =
  let has_guarantee (pol:ast_policy) : bool =
    let ASTPolicy(_,_,_,ropt) = pol in
    match ropt with
      | RMin _
      | RBoth( _, _) -> true
      | _ -> false in
  let ASTProgram(pols) = program in
  List.partition has_guarantee pols

let solve_guaranteed (stmts:statement list) mins maxs v_to_s (topo:topo)
    : flow list =
  match stmts with
  | [] -> []
  | _ -> let module Solver = Merlin_LP.Make(Merlin_LP.ShortestPathHeuristic) in
         let varmap, graphs_nfas = Solver.mk_graph stmts topo mins in
         let edges = Solver.solve graphs_nfas topo in
         let flows = Solver.collect topo edges varmap mins maxs v_to_s in
         if (!verbose) then begin
           let oc = open_out "solution.dot" in
           output_string oc (rated_soln_to_string topo edges varmap);
           close_out oc;
         end;
         flows

(* TODO(basus) Check the regex type here. The host_to_switch only works for src .* dst *)
(* TODO(basus) Only do this if we have a .* style regex. Bucket graphs first *)
let solve_unguaranteed (stmts:statement list) (t:topo) (mins:int64 StringMap.t)
    (maxs:int64 StringMap.t) : flow list =
  let open Merlin_Generate in
  let h_to_s,_,hless = Merlin_Topology.remove_hosts t in
  let module F = Forward(struct
      let topo = t
      let hostless = hless
      let size = Net.Topology.num_vertexes hless
  end) in
  let start = Merlin_Time.time () in
  let flows = begin
    let open Core.Std in
    match stmts with
      | [] -> []
      | stmts ->
        List.fold_left stmts ~init:[] ~f:(fun acc stmt ->
          let Statement(pred,regex,var) = stmt in
          (* Warning: if you get a Not_found failure from here, it probably
             means that there is a problem with the topologies later on. You're
             trying to get node information from a topology that doesn't
             contain that node. *)
          let max =
            try Some (StringMap.find var maxs)
            with Not_found -> None in
          let min =
            try Some (StringMap.find var mins)
            with Not_found -> None in
          let forwards = F.from_regex regex h_to_s min max in
          (pred,forwards)::acc) end in
  let stop = Merlin_Time.time () in
  Merlin_Stats.rless_pathgen := (Int64.sub stop start);
  flows


let solve (ir:policy) (t:topo) : flow list =
  let ir' = Merlin_Preprocess.localize_ir ir in
  let (guarantees, others, mins, maxs, var_to_stmt) = partition ir' in

  if (!verbose) then begin
    Printf.printf "\nMax map:\n";
    StringMap.iter (fun k v -> Printf.printf "%s -> %Ld\n" k v) maxs;
    Printf.printf "Min map:\n";
    StringMap.iter (fun k v -> Printf.printf "%s -> %Ld\n" k v) mins;
    Printf.printf "\n";
  end;

  let guarantees' = List.map pad_stmt guarantees in
  let topo,_,_ = pad_topo t in

  let flows  = solve_guaranteed guarantees' mins maxs var_to_stmt topo in
  let flows' = solve_unguaranteed others t mins maxs in
  (flows@flows')

let solve_sinktree (ap:ast_program) (topo:topo) : flow list =
  let ASTProgram(pols) = ap in

  let prereq = (Merlin_Sinktree.make_prereq topo
                  (Merlin_Topology.is_not_host topo)) in
  let module S = (val prereq : Merlin_Sinktree.TOPO_NFA) in
  let module ST = Merlin_Sinktree.Make(S) in

  let start_time = Merlin_Time.time () in
  let flows = List.fold_left (fun acc p ->
    let open Merlin_NFA in
    let ASTPolicy(c,pred,rx,ro) = p in
    let Foreach(p,e) = c in
    let Expansion(s,srcs,dsts) = e in

    let rx_nfa = NFA.of_regex rx in
    let (src_states,sink_states,topo_nfa) = ST.add_endpoints srcs dsts in

    let start = Merlin_Time.time () in
    let cross_tbl,cross_nfa = ST.cross topo_nfa rx_nfa in
    Merlin_Stats.sink_cross := Merlin_Time.from start;
    Merlin_Stats.cross_states := NFA.size cross_nfa;
    Merlin_Stats.cross_edges := (NFA.EdgeSet.cardinal (NFA.edges cross_nfa));

    if (!verbose) then begin
      NFA.to_dotfile topo_nfa "topo.dot";
      NFA.to_dotfile rx_nfa "regex.dot";
      NFA.to_dotfile cross_nfa "cross.dot";
      (* List.iter (fun (t,_,_,_) -> Merlin_Topology.to_dotfile t "trees.dot") trees *)
    end;

    let start = Merlin_Time.time () in
    let backmap = NFA.backward_mapping cross_nfa in
    let trees = ST.fast_sink_trees cross_nfa backmap in
    Merlin_Stats.tree_count := List.length trees;
    Merlin_Stats.sink_trees := Merlin_Time.from start;

    (* Calculate and store the average BFS time *)
    let total = List.fold_left (fun acc (_,_,t,_) -> Int64.add acc t) 0L trees in
    Merlin_Stats.ave_bfs := Int64.div total (Int64.of_int (List.length trees));
    let total = List.fold_left (fun acc (_,_,_,t) -> acc + t) 0 trees in
    Merlin_Stats.bfs_inner_loop := total/ (List.length trees);

    let irgen_start = Merlin_Time.time () in
    (* Get the ingress switches *)
    let src_hosts = LocationSet.fold (fun l acc ->
      (name_to_node l) :: acc) srcs [] in
    let src_switches = List.fold_left (fun acc h ->
      Net.Topology.VertexSet.add acc (VertexHash.find_exn S.host_to_switch h)
    )  Net.Topology.VertexSet.empty src_hosts in

    let flows = List.fold_left (fun acc (tree,root,_,_) ->
      let hosts = VertexHash.find_exn S.switch_to_hosts root in
      let fwds = ST.generate tree root src_switches hosts in
      (pred,fwds)::acc
    ) acc trees in

    Merlin_Stats.sink_irgen := Merlin_Time.from irgen_start;

    flows
  ) [] pols in
  Merlin_Stats.rless_pathgen := (Int64.sub (Merlin_Time.time ()) start_time);
  flows

let check_invariant (ast:ast_program) (topo:topo) : invariant list =

  let prereq = (Merlin_Sinktree.make_prereq topo
                  (Merlin_Topology.is_not_host topo)) in
  let module T = (val prereq : Merlin_Sinktree.TOPO_NFA) in
  let module ST = Merlin_Sinktree.Make(T) in
  let ASTProgram(pols) = ast in
  List.fold_left (fun acc p ->
    let open Merlin_NFA in
    let open Merlin_Invariants in
    let ASTPolicy(c,pred,rx,ro) = p in
    let Foreach(p,e) = c in
    let Expansion(s,srcs,dsts) = e in

    let results = (only_hosts topo srcs)::acc in
    let results = (only_hosts topo dsts)::results in

    let rx_nfa = NFA.of_regex rx in
    let (src_states, sink_states, topo_nfa) = ST.add_endpoints srcs dsts in
    let cross_tbl, cross_nfa = ST.cross topo_nfa rx_nfa in

    let results = (single_preceding_accept_state cross_nfa)::results in
    let results = (egress_transition_to_preaccept_state cross_nfa
                     (NFA.StateSet.cardinal sink_states))::results in
    let results = (egress_reach_final_state cross_nfa sink_states
                     cross_tbl)::results in
    let results = (single_initial_state cross_nfa)::results in
    let results = (initial_to_all_ingress_states cross_nfa src_states cross_tbl)::results in
    NFA.to_dotfile topo_nfa "topo.dot";
    NFA.to_dotfile rx_nfa "regex.dot";
    NFA.to_dotfile cross_nfa "cross.dot";

    let bm = NFA.backward_mapping topo_nfa in
    let pre_accepts = Hashtbl.find bm (NFA.accept topo_nfa) in
    Printf.printf "Equal sinks and pre-accepts:\t%b\n"
      (NFA.StateSet.cardinal sink_states = Hashset.size pre_accepts);
    results
  ) [] pols


let partition_traffic_classes  (policy_file:string) : ast_program * ast_program =
  let ast = parse_policy_file policy_file in
  let ASTProgram(policies) = ast in
  let (best_effort, guaranteed) = List.partition (fun (ASTPolicy(_,_,_,rate)) -> 
    match rate with
    | RNone -> true
    | RMin(n) -> false
    | RMax(n) -> true
    | RBoth(n, m) -> false
  ) policies in
  (ASTProgram(best_effort), ASTProgram(guaranteed))

let policy_file_to_ir (policy_file:string) : policy option =
  let ast = parse_policy_file policy_file in
  desugar ast

let generate_from_files (tfname:string) (pfname:string) =
  let topo = (parse_topo_file tfname) in
  let flows = solve_sinktree (parse_policy_file pfname) topo in
  Merlin_Generate.from_flows topo flows
