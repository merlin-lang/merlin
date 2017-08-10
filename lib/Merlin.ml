open Frenetic_Packet
open Frenetic_Network

open Merlin_Util
open Merlin_Types
open Merlin_Error
open Merlin_Pretty
open Merlin_Globals
open Merlin_OpenFlow
open Merlin_FrontEnd
open Merlin_BigGraph
open Merlin_Controller
open Merlin_Dictionaries
module PP = Merlin_Preprocess

(* Modes *)
type modeType =
  | CompilerMode
  | ServerMode
  | SinkMode
  | InvariantMode
  | HelpMode

let mode = ref CompilerMode

(* Defaults *)
let heuristic = ref "sp"
let controller = ref false
let negotiator = ref false

let solver = ref false
let dummy = ref false

let lpfile = ref "merlin.lp"

let ip = ref "10.0.0.1"
let port = ref 1337

let from_ir = ref false

let arg_spec =
  [ (* First the operation modes *)
    ("-server",
       Arg.Unit (fun () -> mode := ServerMode),
       "\tExpose a negotiator on the given IP and port")
    ; ("-sink",
       Arg.Unit (fun () -> mode := SinkMode; sink := true),
       "\tGenerate path solutions according to sink trees for egress switches")
    ; ("-inv",
       Arg.Unit (fun () -> mode := InvariantMode),
       "\tRun available invariant checks for the given topology and policy")
    ; ("-help",
       Arg.Unit (fun () -> mode := HelpMode),
       "\tDisplay this list of options")


    (* Then the flags *)
    ; ("-ir",
      Arg.Unit (fun () -> from_ir := true;),
       "\tOperate on the Merlin IR")
    ; ("-verbose",
       Arg.Unit (fun () -> verbose := true),
       "\tverbose printing (useful for debugging)")
    ; ("-longname",
       Arg.Unit (fun () -> shortname := false),
       "\tUse long (human readable) names for the LP.")
    ; ("-controller",
       Arg.Unit (fun () -> controller := true),
       "\tRun as a controller after generating paths.")
    ; ("-solver",
       Arg.Unit (fun () -> solver := true),
       "\tGenerate and solve the LP problem only, do not generate code")
    ; ("-stat",
       Arg.Unit (fun () -> stat := true),
       "\tPrint results in an easy-to-crunch format")
    ; ("-test",
       Arg.Unit (fun () -> test := true),
       "\tPrint results in an unit-test parsable format")
    ; ("-time",
       Arg.Unit (fun () -> timing := true),
       "\tPrint timing information")
    ; ("-mnovs",
       Arg.Unit (fun () -> Merlin_Globals.mnovs := false),
       "\tAssume that the switches are an OpenVSwitch instance in a Mininet network")

    (* And finally the arguments *)
    ; ("-topo",
       Arg.String (fun s -> topo := s; ),
       "\tset topology")
    ; ("-heuristic",
       Arg.String (fun s -> heuristic := s; ),
       "\tset heuristic (sp|reserved|ratio)")
    ; ("-ip",
       Arg.String (fun s -> ip := s),
       "\tspecify a IP address for a negotiator")
    ; ("-port",
       Arg.Int (fun p -> port := p),
       "\tspecify a port for a negotiator")
    ; ("-vlan",
       Arg.Int (fun v ->  dl_vlan := v),
       "\tspecify a value for dl_vlan in the OpenFlow rule")
  ]

let usage =
  Printf.sprintf "usage: %s [OPTIONS] policy" Sys.argv.(0)

let () =
  Arg.parse
    arg_spec
    (fun fn -> policy := fn;)
    usage

let () =
  Gc.set { (Gc.get()) with
    Gc.space_overhead = 500;
    Gc.minor_heap_size = 32 * 1024 * 1024;
    Gc.major_heap_increment = 256 * 1024 * 1024;
    Gc.max_overhead = 700;
  }

let has_files () =
  if ("" = !topo) then
    begin Printf.printf "Must specify topology file\n"; false end
  else if ("" = !policy) then
    begin Printf.printf "Must specify policy file\n"; false end
  else true

let _ =
  if has_files ()
  then match !mode with
    | CompilerMode ->
      let topo = parse_topo_file !topo in
      let Program(pgm) as program = parse_program_file !policy in
      let expanded = PP.expand program in
      let unconst, const = PP.partition expanded in
      let unconstrained, constrained = PP.flatten const, PP.flatten unconst in
      let flows = solve constrained topo in

      (* For policies without rate guarantees *)
      let open Merlin_Generate in
      let h_to_s,_,hless = Merlin_Topology.remove_hosts topo in
      let module F = Forward(struct
          let topo = topo
          let hostless = hless
          let size = Net.Topology.num_vertexes hless
        end) in

      let Policy (stmts, _) = unconstrained in
      let flows' = List.map (fun stmt ->
          let Statement(var,pred,regex) = stmt in
          let rate = StringHash.find var_to_rate var in
          let forwards = F.from_regex regex h_to_s rate in
          (pred,forwards)) stmts in

      let flows = (flows@flows') in
      let (ofs, qcs, tcs, clicks) = Merlin_Generate.from_flows
          topo flows in

      let forwards = Merlin_Generate.Gather.steps ofs in
      let num_ofs = Hashtbl.fold (fun swid pol acc ->
        let flowtable = Merlin_OpenFlow.compile_flowtable swid pol in
        acc + List.length flowtable
      ) forwards 0 in

      Merlin_Stats.step_count := List.length ofs;
      Merlin_Stats.qc_count := List.length qcs;
      Merlin_Stats.of_count := num_ofs;
      Merlin_Stats.tc_count := List.length tcs;

      if (!verbose) then begin
        Merlin_Human.print_all ofs qcs tcs clicks
      end;
      Merlin_Stats.print_stats topo;

      if (!controller) then begin
        let module T = struct
          let forwards = Merlin_Generate.Gather.steps ofs
          let queues = Merlin_Generate.Gather.queues_by_switch qcs
          let ips = Merlin_Topology.switches_to_ips topo
          let infos = Hashtbl.create 1
        end in
        let module Ctrlr = Merlin_Controller.Make(T) in
        ignore (Ctrlr.start 6633);
        Core.never_returns (Async.Scheduler.go ())

      end


    | SinkMode ->
      Printf.printf "Sink mode unimplemented\n%!"
    (* let topo = parse_topo_file !topo in *)
    (* let flows = Merlin_FrontEnd.solve_sinktree *)
    (*   (parse_policy_file !policy) topo in *)

    (* let (steps, qcs, tcs, clicks) = Merlin_Generate.from_flows topo flows in *)

    (* if (!verbose) then begin *)
    (*   Merlin_Human.print_all steps qcs tcs clicks *)
    (* end; *)
    (* Merlin_Stats.print_stats topo; *)
    (* Gc.print_stat stdout *)

  | HelpMode ->
    Printf.printf "Running in Help mode\n";
    Arg.usage arg_spec usage;

  | ServerMode ->
    (* TODO(basus) : This only runs the controller, the "server" still needs to
       be ported to async. *)
    let ofs, qcs, tcs, clicks = [], [], [], [] in
      (* Merlin_FrontEnd.generate_from_files !topo !policy in *)
    let topo = parse_topo_file !topo in
    if (!verbose) then begin
      Merlin_Human.print_all ofs qcs tcs clicks
    end;
    let module T = struct
        let forwards = Merlin_Generate.Gather.steps ofs
        let queues = Merlin_Generate.Gather.queues_by_switch qcs
        let ips = Merlin_Topology.switches_to_ips topo
        let infos = Hashtbl.create 1
    end in
    let module Ctrlr = Merlin_Controller.Make(T) in
    ignore (Ctrlr.start 6633);
    Core.never_returns (Async.Scheduler.go ())

  | InvariantMode ->
    Printf.printf "Invariant mode unimplemented\n%!"
(* let topo = parse_topo_file !topo in *)
(*     let program = parse_program_file !policy in *)
(*     let results = check_invariant program topo in *)
(*     Printf.printf "Name\tResult\tError\n"; *)
(*     List.iter (function Invariant(name,result,error) -> match error with *)
(*       | Some err -> Printf.printf "%s\t%B\t%s\n" name result err *)
(*       | None -> Printf.printf "%s\t%B\tNone\n" name result *)
(*     ) results *)

