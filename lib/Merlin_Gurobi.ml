
open Merlin_Types
open Merlin_Dictionaries
open Merlin_Util
open Merlin_Pretty
open Merlin_LPPretty
open Merlin_LPTypes
open Merlin_Globals
open Merlin_BigGraph
open Frenetic_Network
open Merlin_NFA
open Merlin_Error
open Unix
open Str

(* Static information *)
let lp_file = "merlin.lp"
let sol_file = "merlin.sol"

(***** Gurobi interaction functions *****)

(* Read in solution from the filename, keeping only unassigned variables *)
let read_soln filename =
  let get_line chan =
    try Some(input_line chan)
    with End_of_file -> close_in chan; None in

  let is_usage s =
    let usage = "\\(usage\\)" in
    try
      (Str.search_forward (Str.regexp usage) s 0)  >= 0
    with Not_found -> false in

  let rec loop vars chan = match get_line chan with
    | Some line ->
      if (line.[0] = '#') || (is_usage line) then (loop vars chan)
      else
        let words = split (regexp "[ \t]+") line in
        let edge_name = List.nth words 0 in
        let generate =
          try
            let e = StringHash.find name_to_edge edge_name in
            edge_generatable e
          with Not_found -> false
        in
        if (not generate) then
          (loop vars chan)
        else
          let value = List.nth words 1 in
          if (value = "1") then
            loop ((List.hd words)::vars) chan
          else
            loop vars chan
    | None ->
      vars in

  loop [] (open_in filename)

(* Get the functions that the target node should implement *)
let get_fns t n a =
  let cs = NFA.edge_symbols a in
  NFA.CharSet.fold (fun c acc ->
      if SymbolHash.mem symbol_to_location c then try
          let loc = SymbolHash.find symbol_to_location c in
          let fns = StringHash.find location_to_functions loc in
          LocationSet.fold (fun l acc -> TargetSet.add (Fn l) acc ) fns acc
        with Not_found -> TargetSet.add Phys acc
      else
        acc)
   cs TargetSet.empty

let edges_to_fwds (t:topo) (eas:(Net.Topology.edge * NFA.edge * hop) list)
    (min:int64 option) (max:int64 option) : forward list =
  let open Node in
  let fwd_tbl = VertexHash.create ~size:((List.length eas)/2) () in
  let fn_tbl = VertexHash.create ~size:10 () in

  let update_vertex_in (n:vertex) (p:portId) (h:hop) =
    try
      let (_,op,_,oh) = VertexHash.find_exn fwd_tbl n in
      VertexHash.set fwd_tbl n (p,op,h,oh)
    with Not_found ->
      VertexHash.add_exn fwd_tbl n (p,0l,h,Nohop) in
  let update_vertex_out (n:vertex) (p:portId) (h:hop) =
    try
      let (ip,_,ih,_) = VertexHash.find_exn fwd_tbl n in
      VertexHash.set fwd_tbl n (ip,p,ih,h)
    with Not_found ->
      VertexHash.add_exn fwd_tbl n (0l,p,Nohop,h) in

  let add_function table host f  =
    let set =
      try VertexHash.find_exn table host
      with Not_found -> StringSet.empty
    in
    VertexHash.set table host (StringSet.add f set)
  in

  List.iter (fun (e,a,hop) ->
    let src,srcport = (Net.Topology.edge_src e) in
    let dst,_ = (Net.Topology.edge_dst e) in

    update_vertex_out src srcport hop;
    (* TODO(rjs) look up src and dst, merge on the set that is already there *)
    (* fn_tbl should be storing a string set *)

    let src_fns = get_fns t src a in
    let dst_fns = get_fns t dst a in

    TargetSet.iter (fun fn ->
      match fn with
      | Phys -> let _,dstport = Net.Topology.edge_dst e in
                update_vertex_in dst dstport hop
      | Fn(f) -> add_function fn_tbl dst f
    ) dst_fns ;

    TargetSet.iter (fun fn ->
      match fn with
      | Fn(f) -> add_function fn_tbl src f
      | _ -> ()
    ) src_fns

  ) eas;

  VertexHash.fold fwd_tbl ~init:[] ~f:(fun ~key:n ~data:(ip,op,ih,oh) acc ->
    let label = Net.Topology.vertex_to_label t n in

    let fwd = { device = (Node.device label, Node.id label) ;
                in_hop = ih ; out_hop = oh ;
                in_port = Some ip ; out_port = Some op ; min = min ; max = max;
                topo_vertex = n ; predicate = None ;
                functions = Merlin_Dictionaries.get_fns (Node.name label)
              } in
    fwd :: acc
  )

(* Do all the required bookkeeping and call the Gurobi runtime, keeping all the *)
(*    appropriate measurements *)
let call lp =
  (* Construct the command and remove files from previous runs *)
  let cmd = Printf.sprintf "gurobi_cl ResultFile=%s %s > gurobi_output.log"
    sol_file lp_file in
  if Sys.file_exists lp_file then Sys.remove lp_file;
  if Sys.file_exists sol_file then Sys.remove sol_file;

    (* Write and time file to disk *)
  let lp_start = Merlin_Time.time () in
  Merlin_Util.write_to_file lp_file (Merlin_LPBuffer.buffer_of_lp lp);
  let lp_stop = Merlin_Time.from lp_start in
  Merlin_Stats.lp_write := Merlin_Time.to_nsecs lp_stop;
  if (!verbose) then Printf.printf "LP write time:\t%f\n%!" !Merlin_Stats.lp_write;
    (* Run the gurobi command *)
  let start = Merlin_Time.time () in
  let _ = Sys.command cmd in
  let stop = Merlin_Time.from start in
  Merlin_Stats.gurobi_soln := Merlin_Time.to_nsecs stop;
  if (!verbose) then Printf.printf "Gurobi time:\t%f\n%!" !Merlin_Stats.gurobi_soln;
  (* Read in the solution, keeping only the valid variables *)
  let start = Merlin_Time.time () in
  let soln = read_soln sol_file in
  let stop = Merlin_Time.from start in
  Merlin_Stats.soln_read := Merlin_Time.to_nsecs stop;
  soln

let solution_to_flows
    (t:topo)
    (valids:string list) (varmap:(Net.Topology.edge * NFA.edge * string) StringMap.t)
    (var_to_stmt:(statement) StringMap.t) : flow list =

  (* Map statement variables to their solution variable lists *)
  let group_by_variable id_to_edges name =

    let (_,_,id) = StringMap.find name varmap in
    if (StringMap.mem id id_to_edges) then
      StringMap.add id (name::(StringMap.find id id_to_edges)) id_to_edges
    else
      StringMap.add id [name] id_to_edges in

  (* Is the hop an ingress, egress, both or intermediate hop? *)
  let get_hop ((e,a,v):Net.Topology.edge * NFA.edge * string)
      (x:string) : hop =
    let srcs,dsts =
      try StringHash.find stmt_to_externals v
      with Not_found -> raise (Invalid_statement_id v)
    in

    let s,_ = Net.Topology.edge_src e in
    let d,_ = Net.Topology.edge_dst e in
    let src = (string_of_vertex t s) in
    let dst = (string_of_vertex t d) in
    let is_src = (StringSet.mem src srcs) in
    let is_dst = (StringSet.mem dst dsts) in

    let hop = if (is_src && is_dst) then
        IngressEgress
      else if is_src then
        Ingress
      else if is_dst then
        Egress
      else
        Intermediate in

    if (!verbose) then
      Printf.printf "Edge: %s Statement: %s Hop: %s\n" x v
        (Merlin_Pretty.string_of_hop hop);
    hop in

  let path_to_edges vars =
    if (!verbose) then
    Printf.printf "\nHop and edge mappings for paths:\n\n";
    List.map (fun x ->
      let (e,a,v) = StringMap.find x varmap in
      let h = get_hop (e,a,v) x in
      (e,a,h)) vars in

  let var_paths = List.fold_left group_by_variable StringMap.empty valids in

  let flows = StringMap.fold (fun stmt_var var_path acc ->
    let eas = path_to_edges var_path in
    let Statement(var,pred,regex) = StringMap.find stmt_var var_to_stmt in
    let min, max = from_rate (StringHash.find var_to_rate var) in
    let fwds = edges_to_fwds t eas min max in
    (pred, fwds)::acc
  ) var_paths [] in
  flows
