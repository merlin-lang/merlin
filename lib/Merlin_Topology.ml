open Frenetic_Packet
open Merlin_Types
open Frenetic_Network

module VertexHash = Net.Topology.VertexHash

let pad_topo (n:topo) : topo * vertex * vertex =
  let open Net.Topology in

  let alpha_id = Int64.pred Int64.max_int in
  let na, alpha = Net.Topology.add_vertex n
    (Node.create "_in_" alpha_id Node.Host 1l 1L) in

  let nao, omega = Net.Topology.add_vertex na
    (Node.create "_out_" Int64.max_int Node.Host 11110l 1000L) in
  let port = 1l in

  let edge_label = Link.create 1L Int64.max_int in

  let vs = vertexes nao in

  let n',_ = VertexSet.fold vs ~init:(nao, port) ~f:(fun (acc,port) v ->
    let acc',_ = add_edge acc alpha port edge_label v Int32.max_int in
    (acc', Int32.succ port)
  ) in

  let p = (Int32.pred (Int32.max_int)) in
  let n'',_ = VertexSet.fold vs ~init:(n',port) ~f:(fun (acc,port) v ->
    let acc',_ = add_edge acc v p edge_label omega port in
    (acc',Int32.succ port)
  )  in

  (n'',alpha,omega)

let remove_hosts (n:topo) : (vertex VertexHash.t  * vertex list VertexHash.t * topo) =
  let open Net.Topology in
  let open Node in
  let hosts = VertexSet.filter (vertexes n) ~f:(fun v ->
    match Node.device (vertex_to_label n v) with
      | Host -> true
      | _ -> false
  ) in

  let host_to_switch = VertexHash.create ~size:(VertexSet.length hosts) () in
  let switch_to_host = VertexHash.create ~size:(VertexSet.length hosts) () in

  let hless = fold_edges (fun e acc ->
    let s,sp = (edge_src e) in
    let d,dp = (edge_dst e) in
    let edge_label = edge_to_label n e in
    let s' = Node.device (vertex_to_label n s) in
    let d' = Node.device (vertex_to_label n d) in
    match s',d' with
    | Switch,Switch ->
      let n',s' = add_vertex acc (vertex_to_label n s) in
      let n',d' = add_vertex n' (vertex_to_label n d) in
      let n', e = add_edge n' s' sp edge_label d' dp in n'
      | Switch,_ ->
        let n',v = add_vertex acc (vertex_to_label n s) in
        VertexHash.set host_to_switch d v;
        VertexHash.add_multi switch_to_host v d;
        n'
      | _,Switch ->
        let n', v = add_vertex acc (vertex_to_label n d) in n'
      | _ -> acc
  ) n (empty ()) in

  host_to_switch, switch_to_host, hless

let is_switch t v =
  let node = Net.Topology.vertex_to_label t v in
  match (Node.device node) with
    | Node.Switch -> true
    | _ -> false

let is_host t v =
  let node = Net.Topology.vertex_to_label t v in
  match (Node.device node) with
    | Node.Host -> true
    | _ -> false

let is_not_host t v = not (is_host t v)

let host_to_switch (t:topo) (h:vertex) =
  let open Net.Topology in
  VertexSet.choose (neighbors t h)

let switch_to_hosts (t:topo) (sw:vertex) =
  let open Net.Topology in
  VertexSet.elements (VertexSet.filter (neighbors t sw) ~f:(is_host t))

let hosts (t:topo) : Net.Topology.VertexSet.t =
  let vs = Net.Topology.vertexes t in
  Net.Topology.VertexSet.filter vs ~f:(fun n ->
    let l = Net.Topology.vertex_to_label t n in
    match (Node.device l) with
      | Node.Host -> true
      | _ -> false
  )

let switches (t:topo) : Net.Topology.VertexSet.t =
  let vs = Net.Topology.vertexes t in
  Net.Topology.VertexSet.filter vs ~f:(fun n ->
    let l = Net.Topology.vertex_to_label t n in
    match (Node.device l) with
      | Node.Switch -> true
      | _ -> false
  )

let switches_to_ips (t:topo) :  (switchId, Frenetic_Packet.nwAddr) Hashtbl.t =
  let open Net.Topology in
  let tbl = Hashtbl.create (num_vertexes t) in
  let vs = vertexes t in
  VertexSet.iter vs ~f:(fun v ->
  let l = vertex_to_label t v in
  match (Node.device l) with
    | Node.Switch -> Hashtbl.add tbl (Node.id l) (Node.ip l)
    | _ -> ()) ;
  tbl

let map_endpoints (t:topo) =
  (* ASSUMPTION: The switch-host connections are symmetric, so updated the
     tables for only one case is correct and sufficient. *)
  let open Net.Topology in
  let host_to_switch = VertexHash.create ~size:100 () in
  let switch_to_hosts = VertexHash.create ~size:100 () in
  EdgeSet.iter (edges t) ~f:(fun e ->
    let open Node in
    let s,sp = (edge_src e) in
    let d,dp = (edge_dst e) in
    let s' = Node.device (vertex_to_label t s) in
    let d' = Node.device (vertex_to_label t d) in
    match s',d' with
      | Switch,Host ->
        VertexHash.set host_to_switch d s;
        VertexHash.add_multi switch_to_hosts s d
      | _ -> ()
  ) ;
  (host_to_switch,switch_to_hosts)

let to_dotfile (t:topo) (fname:string) =
  let chan = open_out_gen [Open_append ; Open_creat] 0o666 fname in
  Printf.fprintf chan "%s" (Net.Pretty.to_dot t);
  close_out chan


(* Get the equivalent of vertev v in topology t1 from the topology t2, where
   "equivalent" is defined as having the same label *)
let map_vertex (v:vertex) (t1:topo) (t2:topo) : vertex =
  let label = Net.Topology.vertex_to_label t1 v in
  Net.Topology.vertex_of_label t2 label
