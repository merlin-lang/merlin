open Printf
open Packet
open Network
open SDN_Types
open Merlin_Types
open Merlin_QConfig

open Async.Std

module OF = Async_OpenFlow.OpenFlow0x01.Controller
module Log = Async_OpenFlow.Log

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


module Make (T:TABLES) = struct

  let setup_flow_table (ctrlr:OF.t) (client: OF.Client_id.t) (ft:SDN_Types.flowTable)
      : unit Deferred.t =
    let delete_flows =
      OpenFlow0x01.Message.FlowModMsg OpenFlow0x01_Core.delete_all_flows in
    let to_flow_mod prio flow =
      OpenFlow0x01.Message.FlowModMsg (SDN_OpenFlow0x01.from_flow prio flow) in
    OF.send ctrlr client (5l, delete_flows) >>= fun _ ->
      let priority = ref 65536 in
      Deferred.List.iter ft ~f:(fun flow ->
	if (!Merlin_Globals.verbose) then begin
          Printf.printf "%s\n%!"
            (Merlin_Pretty.string_of_flow flow) end;
	decr priority;
	OF.send ctrlr client (0l, to_flow_mod !priority flow) >>= function
	| `Sent _ ->  return ()
	| `Drop exn -> return () )

  let of_handler (ctrlr:OF.t) event =
    match event with
      |`Connect (cid,feats) ->
        let swid = feats.OpenFlow0x01.SwitchFeatures.switch_id in
        Printf.printf "Switch %Ld connected.\n%!" swid;
        let flow_table =
          try begin
            let policy = (Hashtbl.find T.forwards swid) in
            Printf.printf "Compiling flow table\n%!";
            Merlin_OpenFlow.compile_flowtable swid policy end
          with Not_found ->
            Printf.printf "No flow table found\n%!"; []  in
        setup_flow_table ctrlr cid flow_table >>= (fun _ -> return [])
      | _ -> Printf.printf "Nothing connected\n%!"; return []

  let start (port:int) : unit =
    let open Async_OpenFlow.Stage in
    OF.create port ()
    >>> ( fun ctrlr ->
      let stages = of_handler in
      Printf.printf "Got stages\n%!";
      let r = run stages ctrlr (OF.listen ctrlr) in
      Printf.printf "Run stages\n%!";
      Deferred.don't_wait_for (Pipe.iter r ~f:(fun _ -> return ()))
    )
end
