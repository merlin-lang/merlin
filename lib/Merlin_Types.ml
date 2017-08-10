open Frenetic_Packet
open Merlin_Util
open Frenetic_Network
module SDN_Types = Frenetic_OpenFlow
open Frenetic_OpenFlow

(* Predicate and OpenFlow interop  *)
type queueId = SDN_Types.queueId
type switchId = SDN_Types.switchId
type portId = SDN_Types.portId

type rate = Rate of int64 * int64

type action = SDN_Types.action

type pred =
  | Test of Frenetic_NetKAT.header_val
  | Or of pred * pred
  | And of pred * pred
  | Not of pred
  | Everything
  | Nothing
(* Types for hashes *)

(* dictionary stuff *)
module StringOrd = struct
  type t = string
  let compare = Pervasives.compare
end

module StringSet = Setplus.Make(StringOrd)
module StringMap = Mapplus.Make(StringOrd)

module StringHash = Hashtbl.Make(struct
  type t = string
  let hash = Hashtbl.hash
  let equal = (=)
end)

(** A symbol is what the DPRLE automata use for the alphabet. Each physical *)
(** location should map to exactly one symbol. *)
type symbol = int

type addr = int32

type location = string

module LocationSet = StringSet

module LocationHash = StringHash

(* Regular expressions and utility functions *)
type regex =
  | AnyChar
  | Char of symbol
  | Alt of regex * regex
  | Cat of regex * regex
  | Kleene of regex
  | Group of regex
  | Empty

(* Parser and surface language types *)
type info = (int * int) * (int * int)
type rate_option =
  | RMin of int64
  | RMax of int64
  | RBoth of int64 * int64
  | RNone

(* This should really go somewhere else *)
let from_rate (rate:rate_option) = match rate with
  | RMin r -> Some r, None
  | RMax r -> None, Some r
  | RBoth (r1,r2) -> Some r1, Some r2
  | RNone -> None, None


type operator =
  | Add
  | Sub
  | Mul
  | Div

type expr =
  | Binary of operator * expr * expr
  | RateLiteral of int64

(* type expansion = Expansion of string * LocationSet.t  * LocationSet.t *)


(* type pair = Pair of location * location *)
(* type comprehension = Foreach of pair * expansion *)

(* type ast_policy  = ASTPolicy of comprehension * pred * regex * rate_option *)
(* type ast_program = ASTProgram of ast_policy list *)

type bandwidth =
  | BLit of Int64.t
  | BVar of string
  | BSum of bandwidth * bandwidth

type formula =
  | FMax of bandwidth * Int64.t
  | FMin of bandwidth * Int64.t
  | FAnd of formula * formula
  | FOr of formula * formula
  | FNeg of formula
  | FNone

(** Syntax Types *)

(** Macro syntax types *)
type variable = string

type container = Name of string
               | Set of LocationSet.t

type iterator = Foreach of variable * variable

type expander = Zip of container * container
              | Cross of container * container
              | Distinct of container * container

type block = Block of { iter : iterator
                      ; expander : expander
                      ; pred : pred
                      ; regex : regex
                      ; rate : rate_option }

(** Core syntax types *)
type statement = Statement of variable * pred * regex

type policy = Policy of statement list * formula

type program = Program of { names : StringSet.t StringHash.t
                          ; blocks : block list
                          ; policies : policy list }

(* Map functions to network entities *)
type target =
  | Phys
  | Fn of string

(* Backend Code Generator interface *)

type swqconf = QConf of switchId * portId * queueId * int64 * int64
type hostconf = nwAddr * string

type hop =
  | Ingress
  | Intermediate
  | Egress
  | IngressEgress
  | Nohop

type device = Node.device
type topo = Net.Topology.t
type vertex = Net.Topology.vertex
type edge = Net.Topology.edge

type forward = {
  device      : device * int64       (* Device type and ID *)
  ; topo_vertex : vertex
  ; in_hop    : hop
  ; out_hop   : hop
  ; in_port   : portId option
  ; out_port  : portId option
  ; min      : int64 option
  ; max      : int64 option
  ; predicate : pred option
  ; functions : StringSet.t
}

type transform = forward * string

type flow = pred * forward list

type step = pred * action list * switchId

type instruction =
  | Forward of portId * switchId * portId * (int64*int64) option * hop
  | Throttle of nwAddr * (int64 * int64)
  | Function of string * string list (* hostname * functions *)

type path = instruction list

(* OpenFlow support types *)
type switchInfo = {
  switch_id : SDN_Types.switchId ;
  configure_queues : swqconf list -> unit;
  clear_queues : switchId -> unit;
  flow_table : SDN_Types.flowTable ;
  reader : SDN_Types.flowTable Async.Pipe.Reader.t ;
  writer : SDN_Types.flowTable Async.Pipe.Writer.t ;
}

(* Types for checking invariants *)

type invariant = Invariant of string * bool * string option

(* Various Collection types *)

module PredMap = Map.Make(struct
  type t = pred
  let compare = Pervasives.compare
end)

module PredHash = Hashtbl.Make(struct
  type t = pred
  let hash = Hashtbl.hash
  let equal = (=)
end)

module NodePairHash = Hashtbl.Make(struct
  type t = vertex * vertex
  let hash = Hashtbl.hash
  let equal = (=)
end)

module TargetOrd = struct
  type t = target
  let compare = Pervasives.compare
end

module TargetSet = Set.Make(struct
  type t = TargetOrd.t
  let compare = Pervasives.compare
end)

module Int32Map = Mapplus.Make(Int32)
module Int64Map = Mapplus.Make(Int64)

module Syntax = struct

end
