open Sys
open Frenetic_Packet
open Merlin_Util
open Merlin_Types
open Merlin_Dictionaries
open Frenetic_Network


(* Utility functions *)
let fraction n cap =
  let mbps = (Int64.div (Int64.mul 1000L n) cap) in
  match mbps with
    | 0L -> "1"
    | _ -> Int64.to_string mbps


(* Type definitions for configuration modules *)
module type SWITCHCONFIG = sig
  val ip : string
  val user : string
  val passwd : string
end

module type OVSCONFIG = sig
  val ovs_binary : string
  val qtype : string
  val port2intf : switchId -> portId -> string
  val install : switchId -> string list -> unit
end


(* Modules for parameterizing the OVS configuration for different platforms *)
module ProntoOVS(C:SWITCHCONFIG) = struct

  let ovs_binary =
    Printf.sprintf "/ovs/bin/ovs-vsctl --db=tcp:%s:%d" C.ip 6633

  let qtype = "PRONTO_STRICT"

  let port2intf (sw:switchId) (q:queueId) : string =
    Printf.sprintf "ge-1/1/%ld" q

  let install (sw:switchId) (cmds:string list) : unit =
    (* TODO: figure out a way to set all the configurations with a single SSH
    iteraction (perhaps by writing to a shell script first?) *)
    List.iter (fun confcmd ->
      let cmd = Printf.sprintf "sshpass -p %s ssh %s@%s %s\n"
        C.passwd C.user C.ip confcmd in
      (* Printf.printf "%s" cmd; *)
      let _ = Sys.command cmd in
      ()) cmds

end

module MininetOVS =
struct

  let ovs_binary = "sudo ovs-vsctl"

  let qtype = "linux_htb"

  let port2intf (sw:switchId) (q:queueId) =
    Printf.sprintf "s%Ld-eth%ld" sw q

  let install (sw:switchId) (cmds:string list) : unit =
    let _ = List.map (fun c -> Sys.command ("sudo " ^ c)) cmds in ()

end

(* Functor for generating OVS interaction modules, parameterizing on the
   characteristics of different platforms (specifically, the queue type, the
   mapping from OpenFlow ports to interface names, and the installation
   method) *)
module MakeOVS (OVS:OVSCONFIG) =
struct

  let qoscount = ref 0

  let newqos () : string =
    incr qoscount ; Printf.sprintf "qos%d" !qoscount


  (* This returns one ovs-vsctl per interface that configures the QoS and
     and all queues on that interface *)
  let get_queue_cmd (intfconfs:(string*((queueId*(int64*int64)) list)) list)
      : string list =
    List.map ( fun (intf,qs) ->
      let qosid = newqos () in
      let hdr = Printf.sprintf "%s -- set port %s qos=@%s "
        OVS.ovs_binary intf qosid in
      let qos = Printf.sprintf "-- --id=@%s create qos type=%s " qosid OVS.qtype in
      let qdecls = List.fold_left 
        (fun acc (qid,rate) ->
          acc ^ (Printf.sprintf "queues:%ld=@newq%ld " qid qid)) 
        "" qs in
      let qconfs = List.fold_left (fun acc (qid, (min,max)) ->
        let newq = Printf.sprintf
          "-- --id=@newq%ld create queue other-config:min-rate=%Ld other-config:max-rate=%Ld "
          qid min max in
        acc ^ newq
      ) "" qs in
      hdr ^ qos ^ qdecls ^ qconfs ^ "\n"
    ) intfconfs

  let add_queues
      (tbl: (switchId, (portId, (queueId,(int64 * int64)) Hashtbl.t) Hashtbl.t) Hashtbl.t)
      : unit =
    Hashtbl.iter (fun sw porttbl ->
      let qcmdls = Hashtbl.fold (fun port qtbl acc ->
        let intf = OVS.port2intf sw port in
        let qls = Hashtbl.fold (fun q r acc -> (q,r)::acc) qtbl [] in
        (intf,qls)::acc
      ) porttbl [] in
      OVS.install sw (get_queue_cmd qcmdls)
    ) tbl

  let configure_queues (qcls : swqconf list) : unit =
    let tbl = Merlin_Generate.Gather.queue_configs qcls in
    add_queues tbl

  let clear_queues (sw:switchId) : unit =
    let cmd = Printf.sprintf "%s -- --all destroy QoS -- --all destroy Queue"
      OVS.ovs_binary in
    OVS.install sw [cmd]
end

(* module MakeReference(C:SWITCHCONFIG) = *)
(* struct *)

(*   let add_queue (qc:swqconf) (g:Merlin_Topology.topo) : string = *)
(*     let QConf(s,port,qid,Rate(min,max)) = qc in *)
(*     let sw = Node.Switch s in *)
(*     let dst = Network.next_hop g sw port in *)
(*     let es = Network.find_all_edges g sw dst in *)
(*     let capacity = List.fold_left *)
(*       (fun acc e -> if (Link.srcport e) = port then (Link.capacity e) else acc) *)
(*       131072L es in *)
(*     let smin = fraction min capacity in *)
(*     Printf.sprintf "dpctl add-queue tcp:%s %Ld %Ld %s" *)
(*       C.ip port qid smin *)

(*   let dump_queue (q:queueId) : string = *)
(*     Printf.sprintf "dpctl dump-queue tcp:%s %Ld" C.ip q *)

(*   let configure_queues (qcls: swqconf list) (t: Merlin_Topology.topo) : unit = *)
(*     let _ = List.map (fun qc -> *)
(*       let cmd = add_queue qc t in *)
(*       Sys.command cmd *)
(*     ) in () *)
(* end *)

module Mininet = MakeOVS(MininetOVS)
