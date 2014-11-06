open Merlin_Types
open SDN_Types

type pattern = Pattern.t

val mk_flow : pattern -> group -> flow

val group_union : group -> group -> group

val union : (group -> group -> group) -> flowTable -> flowTable -> flowTable

val compile : pred -> action list -> flowTable

val optimize : flowTable -> flowTable
val compile_flowtable : Int64.t -> (pred * action list) list -> flowTable
