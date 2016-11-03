open Printf
open Frenetic_Packet
open Frenetic_Network
open Frenetic_OpenFlow
open Merlin_Types
open Merlin_QConfig

open Async.Std

module OF10 = Frenetic_OpenFlow0x01
module OF = Frenetic_OpenFlow0x01_Plugin.LowLevel
module Log = Frenetic_Log

module type TABLES = sig
  val forwards : (switchId, (pred * action list) list) Hashtbl.t
  val queues : (switchId, swqconf list) Hashtbl.t
  val ips : (switchId, Frenetic_Packet.nwAddr) Hashtbl.t
  val infos : (switchId, switchInfo) Hashtbl.t
end

module type S = sig
  val start : int -> unit
end

module type MAKE = functor (T:TABLES) -> S


module Make (T:TABLES) = struct

  let setup_flow_table (swid:OF10.switchId) (ft:Frenetic_OpenFlow.flowTable) : unit Deferred.t = 
    let delete_flows =
      OF10.Message.FlowModMsg OF10.delete_all_flows in
    let to_flow_mod prio flow =
      OF10.Message.FlowModMsg (Frenetic_OpenFlow.To0x01.from_flow prio flow) in
    OF.send swid 5l delete_flows >>= fun _ ->
      let priority = ref 65536 in
      Deferred.List.iter ft ~f:(fun flow ->
	if (!Merlin_Globals.verbose) then begin
          Format.printf "%s\n%!"
            (Merlin_Pretty.string_of_flow flow) end;
	decr priority;
	OF.send swid 0l (to_flow_mod !priority flow) >>= fun _ ->
        return ())

  let of_handler (event:Frenetic_OpenFlow.event) =
    match event with
      | SwitchUp (swid,ports) ->
        Format.printf "Switch %Ld connected.\n%!" swid;
        let flow_table =
          try begin
            let policy = (Hashtbl.find T.forwards swid) in
            Format.printf "Compiling flow table\n%!";
            Merlin_OpenFlow.compile_flowtable swid policy end
          with Not_found ->
            Format.printf "No flow table found\n%!"; []  in
        setup_flow_table swid flow_table >>= (fun _ -> return ())
      | _ -> return ()

  let start (port:int) : unit =
    OF.start port;
    Deferred.don't_wait_for (Pipe.iter OF.events of_handler);
end
