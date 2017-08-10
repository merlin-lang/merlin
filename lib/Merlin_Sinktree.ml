open Frenetic_Network
open Merlin_NFA
open Merlin_Types
open Merlin_Dictionaries

module NK = Frenetic_NetKAT

module type TOPO_NFA = sig
  val topo : topo
  val topo_nfa : NFA.t
  val vertex_to_state : NFA.state VertexHash.t
  val vertex_to_trans : symbol VertexHash.t
  val trans_to_vertex : vertex SymbolHash.t
  val state_to_vertex : (NFA.state,vertex) Hashtbl.t
  val host_to_switch  : vertex VertexHash.t
  val switch_to_hosts : vertex list VertexHash.t
  val single_hops     : vertex Hashset.hashset
end

module type S = sig
  open NFA
  val add_endpoints : LocationSet.t -> LocationSet.t ->
    (NFA.StateSet.t * NFA.StateSet.t * NFA.t)
  val cross : NFA.t -> NFA.t -> ((NFA.state,NFA.state list) Hashtbl.t * NFA.t)
  val fast_sink_tree : NFA.t -> NFA.state -> symbol ->
      ((NFA.state, NFA.state Hashset.hashset) Hashtbl.t) -> (topo * int)
  val fast_sink_trees : NFA.t -> (NFA.state, NFA.state Hashset.hashset)
    Hashtbl.t -> (topo * vertex * float * int) list
  val generate : topo -> vertex -> Net.Topology.VertexSet.t -> vertex list ->
    forward list
end

module type MAKE = functor (T:TOPO_NFA) -> S

module Make (S:TOPO_NFA) = struct
  open S

  (* Utility functions *)
  let add_transition (t:topo) src dst : topo =
    let open Net.Topology in
    try
      let src_vertex = SymbolHash.find S.trans_to_vertex src in
      let dst_vertex = SymbolHash.find S.trans_to_vertex dst in
      let edge = find_edge S.topo src_vertex dst_vertex in
      let edge_label = edge_to_label S.topo edge in
      let _, src_port = edge_src edge in
      let _, dst_port = edge_dst edge in
      let t', e' = add_edge t src_vertex src_port edge_label dst_vertex dst_port in
      t'
    with Not_found -> t

  let add_endpoints (srcs:LocationSet.t) (dsts:LocationSet.t) =
    let open Node in

    let topo_nfa = NFA.copy S.topo_nfa in

    let src_hosts = LocationSet.fold (fun l acc ->
      (name_to_node l) :: acc) srcs [] in
    let src_states = List.fold_left (fun acc host ->
      let sw =  (VertexHash.find_exn S.host_to_switch host) in
      NFA.StateSet.add (VertexHash.find_exn vertex_to_state sw) acc
    ) NFA.StateSet.empty src_hosts in

    let _ = NFA.init_all topo_nfa src_states in

    let sink_hosts = LocationSet.fold (fun l acc ->
      (name_to_node l) :: acc) dsts [] in

    let tbl = Hashtbl.create (List.length sink_hosts) in
    let sink_states = List.fold_left (fun acc host ->
      let sw = VertexHash.find_exn host_to_switch host in
      let state = (VertexHash.find_exn vertex_to_state sw) in
      let trans = VertexHash.find_exn S.vertex_to_trans sw in
      Hashtbl.replace tbl state trans;
      NFA.StateSet.add state acc
    ) NFA.StateSet.empty sink_hosts in

    let _ =  NFA.accept_all topo_nfa tbl in

    (src_states, sink_states, topo_nfa)

  (** Perform the cross product between the NFA representing a topology and that
      representing a regular expression.
      @param topo_nfa the NFA representing the physical topology
      @param rx_nfa the NFA representing the regular expression
      @return an NFA representing the cross product of the two and a Hashtbl.t
      mapping states in the topology NFA to states in the cross-product NFA
  **)
  let cross (topo_nfa:NFA.t) (rx_nfa:NFA.t) =
    NFA.intersect topo_nfa rx_nfa (NFA.states topo_nfa)


  (** Perform a backwards breadth-fast from the given state, generating a
      forwarding tree in topology specified in the S module. Assumes that the
      transition symbols in the NFA are nodes in the topology and the required
      mappings are found in the S module.

      @param nfa the NFA on which to perform the BFS
      @param s the state to start the BFS from
      @param backmap a Hashtbl from states in the NFA to a set of that state's
      predecessors
      @return a topology that forwards to the given state s
  **)
  let fast_sink_tree (nfa:NFA.t) (s:NFA.state) (sym:symbol)
      (backmap:(NFA.state, NFA.state Hashset.hashset) Hashtbl.t) : (topo * int) =

    let visited = Hashset.create 100 in
    let next = Queue.create () in

    let rec loop ((s,prev):(NFA.state * symbol)) (t:topo) (i:int) : (topo * int) =
      Hashset.add visited s;
      let t' =
        try
          let predecessors = Hashtbl.find backmap s in
          Hashset.fold (fun s' t' ->
            if Hashset.mem visited s' then
              t'
            else begin
              try
                let symbol = NFA.CharSet.choose (NFA.transitions nfa s' s) in
                Queue.add (s',symbol) next;
                add_transition t' symbol prev
              with Not_found -> Queue.add (s',prev) next ; t'
            end
          ) predecessors t
        with Not_found -> t in
      if Queue.is_empty next then (t',i) else loop (Queue.take next) t' (i+1) in

    let root = Net.Topology.vertex_to_label S.topo
      (SymbolHash.find S.trans_to_vertex sym) in
    let topo, _ = Net.Topology.add_vertex (Net.Topology.empty ())
      root in
    let tree = loop (s,sym) topo 0 in
    tree


  let fast_sink_trees (nfa:NFA.t) (backmap:(NFA.state,NFA.state Hashset.hashset) Hashtbl.t)
      : (topo * vertex * float * int) list =
    let open NFA in
    let bfs_root (nfa:NFA.t) =
      let final = accept nfa in
      let preds = Hashtbl.find backmap final in
      Hashset.choose preds in

    let Invariant(name,inv,msg) = Merlin_Invariants.single_preceding_accept_state nfa in

    if not inv then ( match msg with
      | None -> failwith (Printf.sprintf "Invariant '%s' not satisfied" name)
      | Some m ->
        failwith (Printf.sprintf "Invariant '%s' not satisfied: %s" name m) );

    let root = bfs_root nfa in
    let preds = Hashtbl.find backmap root in
    let rooted_trees = Hashset.fold (fun state acc ->
      let symbol = CharSet.choose (transitions nfa state root) in
      let root = (SymbolHash.find S.trans_to_vertex symbol) in
      let label = Net.Topology.vertex_to_label S.topo root in
      let topo,_ = Net.Topology.add_vertex (Net.Topology.empty ()) label in
      (topo,state,symbol,root)::acc
    ) preds [] in

    let size = Net.Topology.num_vertexes S.topo in
    let loop_times = ref 0 in

    let bfs (state:state) (sym:symbol) (t:topo) =
      let seen = Hashset.create size in
      let next = Queue.create () in
      loop_times := 0;

      let rec loop ((s,prev):(state * Merlin_Types.symbol)) (t:topo) : topo =
        loop_times := !loop_times + 1;
        let t' =
          try
            let predecessors = Hashtbl.find backmap s in
            Hashset.fold (fun s' t' ->
              let state, symbol, t'' =
                try
                  (* WARNING: Assumes only one symbol in the transition set *)
                  let symbol = NFA.CharSet.choose (NFA.transitions nfa s' s) in
                  let t'' = add_transition t' symbol prev in
                  (s', symbol, t'')
                with Not_found -> (s', prev, t') in
              if not (Hashset.mem seen s') then begin
                Hashset.add seen s';
                Queue.add (state, symbol) next end;
              t'
            ) predecessors t
          with Not_found -> t in
        if Queue.is_empty next then t' else loop (Queue.take next) t' in

      loop (state,sym) t in

    List.map (fun (topo,state,symbol,root) ->
      let start = Merlin_Time.time () in
      let result = bfs state symbol topo in
      if Net.Topology.num_vertexes result = 1 then
        Hashset.add S.single_hops root;
      let time = Merlin_Time.from start in
      (result, root, Merlin_Time.to_nsecs time, !loop_times)
    ) rooted_trees


  (**  Generate the backend IR forward instructions that will implement the given
       forwarding tree.
       @param soln The forwarding tree
       @param root The root of the forwarding tree
       @param ingress The set of ingress switches for this forwarding tree
       @param sink_hosts The hosts attached the root node of this tree
       @return A list of forwarding instruction that implement the given
               forwarding tree
  **)
  let generate (soln:topo) (root: vertex)
      (ingress:Net.Topology.VertexSet.t) (sink_hosts:vertex list) : forward list =

    let open Net.Topology in

    (* Generate a predicate that identifies the destination hosts and collect
       the egress switches *)
    let dst_pred_head =
      let ip = Node.ip (Net.Topology.vertex_to_label S.topo (List.hd
       sink_hosts)) in
      Test(NK.IP4Dst(ip,Int32.of_int 32)) in
    let dst_pred = List.fold_left (fun acc host ->
      let ip = Node.ip (Net.Topology.vertex_to_label S.topo host) in
      Or (Test(NK.IP4Dst(ip,Int32.of_int 32)), acc)
    )  dst_pred_head (List.tl sink_hosts) in
    let dst_pred = And( Test (NK.EthType 0x0800), dst_pred) in

    (* Fold over all edges in the forwarding S.topo and generate appropriate
       forwarding instructions *)
    let inter_fwds = EdgeSet.fold (edges soln) ~init:[] ~f:(fun acc edge ->
      let s,sp = edge_src edge in
      let slabel = vertex_to_label soln s in
      let s' = vertex_of_label S.topo slabel in


      (* For each ingress switch, make a single forwarding instruction for all
         the hosts with the destination predicate *)
      if VertexSet.mem ingress s' then
        if (s' = root) then acc
        else
          let _,sp = edge_src edge in
          let fwd = { device = (Node.device slabel, Node.id slabel); topo_vertex = s'
                    ; in_hop = Ingress ; out_hop = Intermediate
                    ; in_port = None ; out_port = Some sp
                    ; min = None ; max = None ; predicate = Some dst_pred
                    ; functions = Merlin_Dictionaries.get_fns (Node.name slabel)
                    } in
          fwd :: acc

      (* For each internal edge, make a forwarding instruction with only
         intermediate hops *)
      else
        let fwd = { device = (Node.device slabel, Node.id slabel) ; topo_vertex = s
                  ; in_hop = Intermediate ; out_hop = Intermediate
                  ; in_port = None ; out_port = Some sp
                  ; min = None ; max = None; predicate = None
                  ; functions = Merlin_Dictionaries.get_fns (Node.name slabel); } in
        fwd::acc
    ) in

    if (num_vertexes soln) = 1 then
      (* If there's only a single switch in the path, don't tag/untag, generate
         a direct forwarding rule *)
      let open SDN_Types in
      List.fold_left (fun acc hdst ->
        let e' = find_edge S.topo root hdst in
        let label = vertex_to_label S.topo root in
        let _,oport = edge_src e' in
        let dstip = Node.ip (vertex_to_label S.topo hdst) in
        let pred = And (Test (NK.EthType 0x0800),
                        Test (NK.IP4Dst (dstip, Int32.of_int 32))) in
        let fwd = { device = (Node.device label, Node.id label) ; topo_vertex = root
                  ; in_hop = Ingress ; out_hop = Egress
                  ; in_port = None ; out_port = Some oport
                  ; min = None ; max = None; predicate = Some pred
                  ; functions = Merlin_Dictionaries.get_fns (Node.name label) } in
        fwd::acc
      ) inter_fwds sink_hosts

    else
      (* For the egress switch (the tree's root) generate per-host forwarding
         actions keyed on the hosts' ip address.*)
      let open SDN_Types in
      let label = vertex_to_label S.topo root in
      let fwds = List.fold_left (fun acc host ->
        let ip = Node.ip (vertex_to_label S.topo host) in
        let p = And (Test (NK.EthType 0x0800),
                     Test (NK.IP4Dst(ip, Int32.of_int 32))) in
        let e = find_edge S.topo root host in
        let _,sp = edge_src e in
        { device = (Node.device label, Node.id label)
        ; topo_vertex = root
        ; in_hop = Intermediate ; out_hop = Egress
        ; in_port = None ; out_port = Some sp
        ; min = None ; max = None; predicate = Some p
        ; functions = Merlin_Dictionaries.get_fns (Node.name label);} :: acc
      ) inter_fwds sink_hosts in
      fwds
end

let make_prereq (topo:topo) (pred:vertex -> bool) : (module TOPO_NFA) =
  let topo_nfa, vertex_to_state, vertex_to_trans, state_to_vertex, trans_to_vertex
    = Merlin_NFA.NFA.of_topology topo pred in
  let (host_to_switch,switch_to_hosts) = Merlin_Topology.map_endpoints topo in

  let module S = struct
    let topo = topo
    let topo_nfa = topo_nfa
    let trans_to_vertex = trans_to_vertex
    let state_to_vertex = state_to_vertex
    let vertex_to_state = vertex_to_state
    let vertex_to_trans = vertex_to_trans
    let host_to_switch  = host_to_switch
    let switch_to_hosts = switch_to_hosts
    let single_hops     = Hashset.create 10
  end in
  (module S : TOPO_NFA)

let make (topo:topo) (pred:vertex -> bool) : (module S) =
  let prereq = make_prereq topo pred in
  let module P = (val prereq : TOPO_NFA) in
  let module ST = Make(P) in
  (module ST : S)

