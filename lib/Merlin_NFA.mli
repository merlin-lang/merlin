open Frenetic_Network
open Merlin_Types
open Merlin_Topology
open Merlin_Dictionaries

val is_any_path : regex -> bool
val is_complete_path : regex -> bool
val get_endpoints : regex -> (string * string)

module NFA : sig

  (* Types *)
  type t
  type state
  type edge

  (* Collections *)
  module StateSet : Set.S with type elt = state
  module EdgeSet : Set.S with type elt = edge
  module CharSet : sig
    type t
    val empty : unit -> t
    val mem : t -> symbol -> bool
    val iter : (int -> unit) -> t -> unit
    val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
    val choose : t -> int
    val cardinal : t -> int
  end

  (* Operations on the entire NFA *)
  val size             : t -> int
  val copy             : t -> t
  val forward_fold     : (state -> 'a -> 'a) -> t -> state -> 'a -> 'a
  val subseteq         : t -> t -> bool
  val backward_mapping : t -> (state, state Hashset.hashset) Hashtbl.t
  val backward_reachable : t -> state -> state Hashset.hashset
  val elim_dead_states : t -> unit
  val eps_eliminate    : t -> t

  (* Edge operations *)
  val edge_symbols : edge -> CharSet.t
  val edge_name    : edge -> string
  val edge_src     : edge -> state
  val edge_dst     : edge -> state
  val edge_src_dst : edge -> state * state

  (* State operations *)
  val state_name   : state -> string
  val state_equal  : state -> state -> bool
  val state_accept : t -> state -> bool
  val state_accept_eps : t -> state -> bool
  val state_init   : t -> state -> bool

  (* Accessors *)
  val states      : t -> StateSet.t
  val edges       : t -> EdgeSet.t
  val inits       : t -> StateSet.t
  val initial     : t -> state
  val accept      : t -> state
  val accept_eps  : t -> StateSet.t
  val outgoing    : t -> state -> EdgeSet.t
  val outgoing_eps    : t -> state -> EdgeSet.t
  val transitions : t -> state -> state -> CharSet.t

  (* Mutators *)
  val init_all       : t -> StateSet.t -> state
  val accept_all     : t -> (state, Merlin_Types.symbol) Hashtbl.t -> state
  val eps_accept_all : t -> StateSet.t -> state
  val add_hops       : t -> (state, Merlin_Types.symbol) Hashtbl.t -> (state,(state * Merlin_Types.symbol)) Hashtbl.t
  val intersect      : t -> t -> StateSet.t -> (state, state list) Hashtbl.t * t

  (* Pretty printers *)
  val to_dot     : t -> string
  val to_dotfile : t -> string -> unit
  val to_string  : t -> string

  (* Generators *)
  val of_regex    : regex -> t
  val of_topology : topo -> (vertex -> bool)
    -> (t * state VertexHash.t * Merlin_Types.symbol VertexHash.t * (state,vertex) Hashtbl.t * vertex SymbolHash.t)

end
