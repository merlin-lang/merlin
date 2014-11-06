open Network
open Merlin_Types
open Merlin_Globals
open Merlin_Controller

module type CONFIG =
sig
  val socket : Lwt_unix.file_descr
  val topo : topo
  val idtbl : (switchId, Packet.nwAddr) Hashtbl.t
end

module Make(C:CONFIG) =
struct
  let rec control stream =
    match_lwt Lwt_stream.get stream with
      | Some (ofs,qcs,idtbl,topo) ->

        (* TODO: Move the flow table compilation into the per-switch controller
           threads. This thread should just chop up the (pattern,action)
           configuration and pass it on to each controller thread. *)
        let swtbls = Merlin_Generate.Gather.steps ofs in
        Hashtbl.iter (fun s ofs ->
          let sw = VInt.Int64 s in
          if not (Hashtbl.mem swtbl sw) then ();
          let push = (Hashtbl.find swtbl sw).flow_table_push in
          let flow_table = Merlin_Controller.compile_flowtable sw ofs in
          push (Some (flow_table,qcs))
        ) swtbls;
        control stream
      | None -> Printf.printf "No program\n%!"; control stream

  let rec receive stream push =
    Printf.printf "Waiting to receive\n%!";
    lwt str = Merlin_Util.lwt_recv_all C.socket in
    let pol = Merlin_FrontEnd.parse_policy_string str in
    if (!verbose) then Printf.printf "Received: %s\n%!" str;
    Printf.printf "Policy parsed\n%!";
    (* let pol = Merlin_Preprocess.preprocess pol C.topo in *)
    (* let topo,_,_ = Merlin_Network.pad_topo C.topo in *)
    (* let module Solver = Merlin_LP.Make(Merlin_LP.ShortestPathHeuristic) in *)
    (* let edges, varmap = Solver.solve pol topo in *)
    (* let predmap = Merlin_Gurobi.to_instrs edges varmap pol in *)
    (* let (ofs, qcs, tcs, clicks) = *)
    (*   Merlin_Generate.from_predmap predmap topo C.nametbl *)
    (* in *)
    (* Printf.printf "Going to push\n%!"; *)
    let ofs = [] in
    let qcs = [] in
    push (Some (ofs,qcs,C.idtbl,C.topo));
    receive stream push
end


module MakeIR(C:CONFIG) =
struct
  let rec control stream =
    match_lwt Lwt_stream.get stream with
      | Some (ofs,qcs,idtbl,topo) ->
        let swtbls = Merlin_Generate.Gather.steps ofs in
        Hashtbl.iter (fun s ofs ->
          let sw = VInt.Int64 s in
          if not (Hashtbl.mem swtbl sw) then ();
          let push = (Hashtbl.find swtbl sw).flow_table_push in
          let flow_table = Merlin_Controller.compile_flowtable sw ofs in
          push (Some (flow_table,qcs))
        ) swtbls;
        control stream
      | None -> Printf.printf "No program\n%!"; control stream

  let rec receive stream push =
    Printf.printf "Watiting to receive an IR policy\n%!";
    lwt str = Merlin_Util.lwt_recv_all C.socket in
    let ir_pol = Merlin_FrontEnd.parse_ir_string str in
    if (!verbose) then Printf.printf "Received: %s\n%!" str;
    Printf.printf "Policy parsed\n%!";
    let edges, varmap, local_pol, cbar =
      Merlin_FrontEnd.solve_from_ir C.topo ir_pol in
    let pred_to_instrs = Merlin_Gurobi.to_instrs edges varmap local_pol
      (Hashtbl.create 1) in
    let (ofs, qcs, tcs, clicks) =
      Merlin_Generate.from_predmap pred_to_instrs C.topo in
    let ofs' = Merlin_Generate.from_unrated cbar C.topo in
    Printf.printf "Compilation complete\n%!";
    push (Some (ofs@ofs',qcs,C.idtbl,C.topo));
    receive stream push
end
