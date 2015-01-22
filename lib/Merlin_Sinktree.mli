open Merlin_NFA
open Merlin_Types
open Merlin_Dictionaries
open Network_Common

module type TOPO_NFA = sig
  val topo            : topo
  val topo_nfa        : NFA.t
  val vertex_to_state : NFA.state Net.Topology.VertexHash.t
  val vertex_to_trans : symbol Net.Topology.VertexHash.t
  val trans_to_vertex : vertex SymbolHash.t
  val state_to_vertex : (NFA.state,vertex) Hashtbl.t
  val host_to_switch  : vertex Net.Topology.VertexHash.t
  val switch_to_hosts : vertex list Net.Topology.VertexHash.t
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
    Hashtbl.t -> (topo*vertex*int64 * int) list
  val generate : topo -> vertex -> Net.Topology.VertexSet.t -> vertex list ->
    forward list
end

module type MAKE = functor (T:TOPO_NFA) -> S
module Make : MAKE

val make_prereq : topo -> (vertex -> bool) -> (module TOPO_NFA)
val make : topo -> (vertex -> bool) -> (module S)
