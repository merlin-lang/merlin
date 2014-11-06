(* Various invariants that should be satisfied in different parts of the code *)
open Merlin_NFA
open Merlin_Types
open Merlin_Dictionaries

(* Some Exceptions for better error reporting *)

exception No_ingress_state
exception Multiple_ingress_states
exception Multiple_accept_states
exception Multiple_preceding_states
exception Insufficient_egress_transitions
exception Unequal_state_count
exception Unexpected_epsilon

let count = 4

(* The given LocationSet only contains hosts in the given topology *)
let only_hosts (topo:topo) (locs:LocationSet.t) : invariant =
  let result = LocationSet.fold (fun l acc ->
    let vertex = name_to_node l in
    (Merlin_Topology.is_host topo vertex) && acc) locs true in
  Invariant("Sources are hosts", result, None)

let only_switches (topo:topo) (locs:LocationSet.t) : invariant =
  let result = LocationSet.fold (fun l acc ->
    let vertex = name_to_node l in
  (Merlin_Topology.is_switch topo vertex) && acc) locs true in
  Invariant("Destinations are hosts", result, None)

(* Accept state of the NFA has a single preceding NFA *)
let single_preceding_accept_state (nfa:NFA.t) : invariant =
  let open NFA in
  let name = "Single Preceding Accept State" in
  try
    let accept = accept nfa in
    let preceding = Hashtbl.find (backward_mapping nfa) accept in
    let single_preceding = if (Hashset.size preceding = 1)
      then true else raise Multiple_preceding_states in
    Invariant(name, single_preceding, None)
  with
    | Multiple_preceding_states -> Invariant (name,false, Some "Multiple preceding states")

(* This invariant assumes that the "single preceding accept state" invariant
   holds. In that cases, the number of incoming transitions should be equal to
   the number of egress switches. *)
let egress_transition_to_preaccept_state (nfa:NFA.t) (num_sinks:int) : invariant =
  let open NFA in
  let name = "Egress switches transition to preaccept state" in
  try
    let bm = backward_mapping nfa in
    let accept = accept nfa in
    let preaccept = Hashset.choose (Hashtbl.find bm accept) in
    let sinks = Hashtbl.find bm preaccept in
    let incoming = Hashset.fold (fun state acc ->
      try acc + (CharSet.cardinal (transitions nfa state preaccept))
      with Not_found -> raise Unexpected_epsilon
    ) sinks 0  in
    Printf.printf "Hashset size:\t%d\n" (Hashset.size sinks);
    Printf.printf "Incoming transitions:\t%d\n" incoming;
    Printf.printf "Expected sinks:\t%d\n" num_sinks;
    let all_sinks = if (incoming >= num_sinks)
      then true else raise Insufficient_egress_transitions in
    Invariant(name, all_sinks, None)
  with
    | Insufficient_egress_transitions -> Invariant(name,false,
                                            Some "Insufficient transitions for egress switches")
    | Unexpected_epsilon -> Invariant(name,false, Some "Unexpected Epsilon")

(* This invariant is weaker than the previous one. For all egress states, at
   least one of its corresponding states in the cross product NFA should be
   able to reach the final state of the cross product NFA. *)
let egress_reach_final_state (nfa:NFA.t) (egress:NFA.StateSet.t)
    (tbl:(NFA.state,NFA.state list) Hashtbl.t) : invariant =
  let open NFA in
  let name = "Each egress switch can reach accept state" in
  let reachable = backward_reachable nfa (accept nfa) in
  let all_reach = StateSet.fold (fun state acc ->
    let states = Hashtbl.find tbl state in
    List.exists (fun s -> Hashset.mem reachable s) states && acc
  ) egress true in
  if all_reach then Invariant(name, true, None)
  else Invariant(name, false, Some "Egress cross state does not reach final state")

(* NFA has a single initial state *)
let single_initial_state (nfa:NFA.t) : invariant =
  let open NFA in
  let name = "Single Initial State" in
  let inits = inits nfa in
  if (StateSet.cardinal inits = 1) then Invariant(name, true, None)
  else Invariant(name, false,  Some "Multiple init states")

(* The initial state of the NFA transitions to a state in the cross table entry
   for each state in the ingress set. *)
let initial_to_all_ingress_states (nfa:NFA.t) (ingress:NFA.StateSet.t)
    (cross_tbl:(NFA.state, NFA.state list) Hashtbl.t) : invariant =
  let open NFA in
  let name = "Initial to all Ingress States" in
  let init = initial nfa in
  let init_edges = outgoing_eps nfa init in
  try
    let first_hops = StateSet.fold (fun s acc ->
      let cross = Hashtbl.find cross_tbl s in
      let acc', times = EdgeSet.fold (fun e (acc,times) ->
        if List.mem (edge_dst e) cross then (StateSet.add s acc, times + 1)
        else (acc,times)
      ) init_edges (acc,0) in
      if times = 1 then acc'
      else if times > 1 then raise Multiple_ingress_states
      else raise No_ingress_state
    ) ingress StateSet.empty in
    if (StateSet.cardinal first_hops = StateSet.cardinal ingress)
    then Invariant(name, true, None)
    else raise Unequal_state_count
  with
    | No_ingress_state -> Invariant(name, false, Some "No ingress state")
    | Multiple_ingress_states -> Invariant(name, false, Some "Multiple ingress states")
    | Unequal_state_count -> Invariant(name, false, Some "Unequal state counts")
