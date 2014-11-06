open Merlin_Types
module type TABLES = sig
  val forwards : (switchId, (pred * action list) list) Hashtbl.t
  val queues : (switchId, swqconf list) Hashtbl.t
  val ips : (switchId, Packet.nwAddr) Hashtbl.t
  val infos : (switchId, switchInfo) Hashtbl.t
end

module type S = sig
  val start : int -> unit
end

module type MAKE = functor (T:TABLES) -> S
module Make : MAKE
