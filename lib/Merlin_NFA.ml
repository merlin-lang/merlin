open Pervasives
open Frenetic_Network
open Merlin_Util
open Merlin_Types
open Merlin_Globals
open Merlin_Topology
open Merlin_Dictionaries

let is_any_path (r:regex) = match r with
  | Cat(r1,r2) -> begin
    match r1,r2 with
      | Cat(r1',r2'),Char(_) -> begin
        match r1',r2' with
          | Char(_),Kleene(AnyChar) -> true
          | _ -> false end
      | _ -> false end
  | _ -> false

let rec is_complete_path (r:regex) = match r with
  | Char(_) -> true
  | Cat(r1,r2) -> (is_complete_path r1) && (is_complete_path r2)
  | Empty -> true
  | Group(r) -> is_complete_path r
  | _ -> false

let get_endpoints (r:regex) : (string * string) = match r with
  | Cat(r1,r2) -> begin
    match r1,r2 with
      | Cat(r1',r2'),Char(dst) -> begin
        match r1',r2' with
          | Char(src),Kleene(AnyChar) -> (symbol_to_string src, symbol_to_string dst)
          | _ -> failwith "No distinct endpoints" end
      | _ -> failwith "No distinct endpoints" end
  | _ -> failwith "No distinct endpoints"

module NFA = struct
  open Nfa

  type t = nfa

  type state = Nfa.state
  type edge = state * Nfa.charset option * state

  module EdgeSet = Set.Make(struct
    type t = edge
    let compare = Pervasives.compare
  end)

  module StateSet = Nfa.StateSet

  module CharSet = struct
    type t = Charset.set
    let empty () = Charset.create_empty ()
    let mem = Charset.mem
    let iter = Charset.iter
    let fold = Charset.fold
    let choose = Charset.choose
    let cardinal = Charset.size
  end

  let copy = copy_nfa
  let forward_fold = forward_fold_nfa

  let size nfa = Hashset.size nfa.q
  let state_name s = Printf.sprintf "%d" s

  let to_dot m =
    (* Printf.printf "Src: %s\n" (state_name m.s); *)
    nfa_to_dot m

  let state_equal q1 q2 =
    q1 = q2

  let state_accept_eps m q =
    StateSet.mem m.f (eps_closure m q)

  let state_accept m q =
    m.f = q

  let state_init m q =
    m.s = q

  let to_string m =
    forward_fold_nfa
      (fun q acc ->
        Hashtbl.fold (fun q' ns acc ->
          CharSet.fold (fun n acc ->
            try
              let string_of_state q =
                if state_init m q then "<q" ^ string_of_int q ^ ">"
                else if state_accept m q then "[q" ^ string_of_int q ^ "]"
                else " q" ^ string_of_int q ^ " " in
              let (s,fo) = SymbolHash.find symbol_to_code n in
              acc ^ (Printf.sprintf "%s ==(%s,%s)==> %s\n"
                       (string_of_state q)
                       s
                       (match fo with None -> "_" | Some f -> f)
                       (string_of_state q'))
            with Not_found ->
              acc ^ (Printf.sprintf "q%d - ?? -> q%d\n" q q'))
            ns acc)
          (all_delta m.delta q) acc)
      m m.s "\n"

  let edge_symbols (_,ns,_) = match ns with
    | Some s -> s
    | None -> CharSet.empty ()

  let edge_name (q1,_,q2) =
    Printf.sprintf "%d_%d" q1 q2

  let edge_src_dst (q1,_,q2) =
    (q1,q2)

  let edge_src (q,_,_) =
    q

  let edge_dst (_,_,q) =
    q

  let states m =
    Hashset.fold StateSet.add m.q StateSet.empty

  let inits m =
    StateSet.singleton m.s

  let accept_eps m =
    StateSet.filter (state_accept_eps m) (states m)

  let accept m =
    m.f

  let initial m =
    m.s

  let edges m =
    forward_fold_nfa
      (fun q acc ->
        Hashtbl.fold
          (fun q' ns acc -> EdgeSet.add (q,Some ns,q') acc)
          (all_delta m.delta q) acc)
      m m.s EdgeSet.empty

  let outgoing m q =
    Hashtbl.fold (fun q' ns acc ->
      EdgeSet.add (q,Some ns,q') acc)
      (all_delta m.delta q)
      EdgeSet.empty

  let outgoing_eps m q =
    let non_eps = outgoing m q in
    Hashset.fold (fun q' acc ->
      EdgeSet.add (q,None,q') acc
    ) (Hashtbl.find m.epsilon q) non_eps


  let add_trans = Nfa.add_trans

  let add_hops (m:t) (ss:(state,Merlin_Types.symbol) Hashtbl.t)
      : (state,(state*Merlin_Types.symbol)) Hashtbl.t =
    let tbl = Hashtbl.create (Hashtbl.length ss) in
    Hashtbl.iter (fun s t ->
      let st = new_state m in
      add_trans m s (Character t) st;
      Hashtbl.add tbl s (st,t)
    ) ss;
    tbl

  let accept_all (m:t) (ss:(state,Merlin_Types.symbol) Hashtbl.t) : state =
    let sink = new_state m in
    Hashtbl.iter (fun s t ->
      add_trans m s (Character t) sink;
    ) ss;
    m.f <- sink;
    sink

  let eps_accept_all (m:t) (sl:StateSet.t) : state =
    let sink = new_state m in
    StateSet.iter (fun s ->
      add_trans m s Epsilon sink;
    ) sl;
    m.f <- sink;
    sink

  let init_all (m:t) (ss:StateSet.t) : state =
    let source = new_state m in
    StateSet.iter (fun s ->
      add_trans m source Epsilon s;
    ) ss;
    m.s <- source;
    source

  let eps_eliminate m =
    let qi,qf = 0,1 in
    let m' = new_nfa_states qi qf in
    let h_eps = Hashtbl.create 17 in
    let h_r = Hashtbl.create 17 in
    let () =
      Hashset.iter
        (fun q ->
          let qs = eps_closure m q in
          Hashtbl.add h_eps q qs;
          if q = m.s then
            Hashtbl.add h_r qs qi
          else if StateSet.mem m.f qs then
            let r = new_state m' in
            Hashtbl.add h_r qs r;
            add_trans m' r Epsilon qf
          else if not (Hashtbl.mem h_r qs) then
            let r = new_state m' in
            Hashtbl.add h_r qs r)
        m.q in
    let () =
      forward_fold_nfa
        (fun q () ->
          let qs = Hashtbl.find h_eps q in
          let r = Hashtbl.find h_r qs in
          StateSet.iter
            (fun qi ->
              Hashtbl.iter
                (fun q' ns ->
                  let qs' = Hashtbl.find h_eps q' in
                  let r' = Hashtbl.find h_r qs' in
                  add_set_trans m' r ns r')
                (all_delta m.delta qi))
            qs)
        m m.s () in
    m'

  let transitions (nfa:t) (src:state) (dst:state) : CharSet.t =
    Hashtbl.find (Hashtbl.find nfa.delta src) dst

  let subseteq = nfa_subseteq

  let intersect n1 n2 s =
    let h = Hashset.create (StateSet.cardinal s) in
    StateSet.iter (fun s -> Hashset.add h s) s;
    Nfa.intersect n1 n2 h

  let elim_dead_states = elim_dead_states

  let of_regex r =
    let open Nfa in
    let create () = new_nfa_states 0 1 in
    let rec loop (r:regex) : t =
      match r with
      | AnyChar ->
        let m = create () in
        add_all_trans m m.s m.f;
        m
      | Char n ->
        let m = create () in
        add_trans m m.s (Character n) m.f;
        m
      | Alt(r1,r2) ->
        simple_union (loop r1) (loop r2)
      | Cat(r1,r2) ->
        simple_concat (loop r1) (loop r2)
      | Kleene(r) ->
        let m = loop r in
        add_trans m m.s Epsilon m.f;
        add_trans m m.f Epsilon m.s;
        m
      | Group(r) ->
        loop r
      | Empty ->
        create () in
    let m = eps_eliminate (loop r) in
    elim_dead_states m;
    m

  let of_topology (t:topo) (p:vertex -> bool)
      : (t * state VertexHash.t * Merlin_Types.symbol VertexHash.t * (state,vertex) Hashtbl.t * vertex SymbolHash.t) =
    let open Node in
    let nfa = new_nfa_states 0 1 in

    let num = Net.Topology.num_vertexes t in

    let vertex_to_state = VertexHash.create ~size:num () in
    let vertex_to_symbol = VertexHash.create ~size:num () in
    let symbol_to_vertex = SymbolHash.create num in
    let state_to_vertex = Hashtbl.create num in

    Net.Topology.iter_vertexes (fun n ->
      if p n then begin
        let state = new_state nfa in
        let label = Net.Topology.vertex_to_label t n in
        let code = (Node.name label, None) in
        let sym = get_symbol code in

        (* add_trans nfa state symbol state; *)
        VertexHash.add_exn vertex_to_symbol n sym;
        SymbolHash.add symbol_to_vertex sym n;
        VertexHash.add_exn vertex_to_state n state;
        Hashtbl.add state_to_vertex state n; end
    ) t ;

    Net.Topology.iter_edges (fun e ->
      let src,_ = Net.Topology.edge_src e in
      let dst,_ = Net.Topology.edge_dst e in
      if p src && p dst then begin
        let n_src = VertexHash.find_exn vertex_to_state src in
        let n_dst = VertexHash.find_exn vertex_to_state dst in
        let symbol = VertexHash.find_exn vertex_to_symbol src in
        add_trans nfa n_src (Character symbol) n_dst end
    ) t ;

    nfa, vertex_to_state, vertex_to_symbol, state_to_vertex, symbol_to_vertex

  let to_dotfile (nfa:t) (fname:string) : unit =
    let chan = open_out_gen [Open_append; Open_creat] 0o666 fname in
    Printf.fprintf chan "%s" (to_dot nfa);
    close_out chan

  let backward_mapping = Nfa.backward_mapping
  let backward_reachable = Nfa.backward_reachable
end
