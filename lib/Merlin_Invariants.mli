open Merlin_NFA
open Merlin_Types

(* Exceptions for better error reporting *)

exception Multiple_ingress_states

(* Miscellaneous stuff *)
val count : int                         (* How many invariants are there?
                                           Useful for priming Hashtbls etc. *)

(* Invariant checking functions *)

val only_hosts : topo -> LocationSet.t -> invariant

val single_preceding_accept_state : NFA.t -> invariant

val egress_transition_to_preaccept_state : NFA.t -> int -> invariant

val egress_reach_final_state : NFA.t -> NFA.StateSet.t ->
  (NFA.state,NFA.state list) Hashtbl.t -> invariant

val single_initial_state : NFA.t -> invariant

val initial_to_all_ingress_states : NFA.t -> NFA.StateSet.t -> (NFA.state, NFA.state list) Hashtbl.t -> invariant
