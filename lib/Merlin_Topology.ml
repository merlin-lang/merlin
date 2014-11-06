open Packet
open Merlin_Types
open Network_Common

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

  let n',_ = VertexSet.fold (fun v (acc,port) ->
    let acc',_ = add_edge acc alpha port edge_label v Int32.max_int in
    (acc', Int32.succ port)
  ) vs (nao, port) in

  let p = (Int32.pred (Int32.max_int)) in
  let n'',_ = VertexSet.fold (fun v (acc,port) ->
    let acc',_ = add_edge acc v p edge_label omega port in
    (acc',Int32.succ port)
  ) vs (n',port) in

  (n'',alpha,omega)

let remove_hosts (n:topo) : (vertex VertexHash.t  * vertex VertexHash.t * topo) =
  let open Net.Topology in
  let open Node in
  let hosts = VertexSet.filter (fun v ->
    match Node.device (vertex_to_label n v) with
      | Host -> true
      | _ -> false
  ) (vertexes n) in

  let host_to_switch = VertexHash.create (VertexSet.cardinal hosts) in
  let switch_to_host = VertexHash.create (VertexSet.cardinal hosts) in

  let hless = fold_edges (fun e acc ->
    let s,sp = (edge_src e) in
    let d,dp = (edge_dst e) in
    let edge_label = edge_to_label n e in
    let s' = Node.device (vertex_to_label n s) in
    let d' = Node.device (vertex_to_label n d) in
    match s',d' with
      | Switch,Switch -> let n', e = add_edge acc s sp edge_label d dp in n'
      | Switch,_ ->
        let n',v = add_vertex acc (vertex_to_label n s) in
        VertexHash.replace host_to_switch d v;
        VertexHash.add switch_to_host v d;
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
  VertexSet.elements (VertexSet.filter (is_host t) (neighbors t sw))

let hosts (t:topo) : Net.Topology.VertexSet.t =
  let vs = Net.Topology.vertexes t in
  Net.Topology.VertexSet.filter (fun n ->
    let l = Net.Topology.vertex_to_label t n in
    match (Node.device l) with
      | Node.Host -> true
      | _ -> false
  ) vs

let switches (t:topo) : Net.Topology.VertexSet.t =
  let vs = Net.Topology.vertexes t in
  Net.Topology.VertexSet.filter (fun n ->
    let l = Net.Topology.vertex_to_label t n in
    match (Node.device l) with
      | Node.Switch -> true
      | _ -> false
  ) vs

let switches_to_ips (t:topo) :  (switchId, Packet.nwAddr) Hashtbl.t =
  let open Net.Topology in
  let tbl = Hashtbl.create (num_vertexes t) in
  let vs = vertexes t in
  VertexSet.iter (fun v ->
  let l = vertex_to_label t v in
  match (Node.device l) with
    | Node.Switch -> Hashtbl.add tbl (Node.id l) (Node.ip l)
    | _ -> ()) vs ;
  tbl

let map_endpoints (t:topo) =
  (* ASSUMPTION: The switch-host connections are symmetric, so updated the
     tables for only one case is correct and sufficient. *)
  let open Net.Topology in
  let host_to_switch = VertexHash.create 100 in
  let switch_to_hosts = VertexHash.create 100 in
  EdgeSet.iter (fun e ->
    let open Node in
    let s,sp = (edge_src e) in
    let d,dp = (edge_dst e) in
    let s' = Node.device (vertex_to_label t s) in
    let d' = Node.device (vertex_to_label t d) in
    match s',d' with
      | Switch,Host ->
        VertexHash.replace host_to_switch d s;
        VertexHash.add switch_to_hosts s d
      | _ -> ()
  ) (edges t);
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
