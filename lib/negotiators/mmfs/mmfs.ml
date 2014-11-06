open Merlin_Types
open Sharer
open Pervasives

type input =
  | CmdLine
  | File
  | Unspec

type mode =
  | Once
  | Server
  | IRServer
  | Default

let len = ref 0
let min = ref 1048576L
let max = ref 1073741824L
let ipinput = ref Unspec
let runmode = ref Default
let polfile = ref ""
let demfile = ref ""
let cport = ref 1337
let dport = ref 7331
let policy = ref (Merlin_Types.ASTProgram [])

let arg_spec =
  [
    ("-num",
     Arg.Int (fun i -> len := i),
     "\tSet the number of flows to allocate default bandwidth for."
    )
    ; ("-min",
     Arg.Int (fun i -> min := (Int64.of_int i)),
     "\tSet the minimum bandwidth to share."
    )
    ; ("-max",
     Arg.Int (fun i -> max := (Int64.of_int i)),
     "\tSet the maximum bandwidth to share."
    )
    ; ("-once",
     Arg.Unit (fun () -> runmode := Once),
     "\tRun the fair-sharing algorithm once. Needs -policy and -demands."
    )
    ; ("-server",
     Arg.Unit (fun () -> runmode := Server),
     "\tRun the fair-sharing negotiator as a server."
    )
    ; ("-ir",
     Arg.Unit (fun () -> runmode := IRServer),
     "\tRun the fair-sharing negotiator as a server on IR policies."
    )
    ; ("-policy",
       Arg.String (fun s -> polfile := s),
       "\tSpecify a file from which to read the initial policy."
    )
    ; ("-demands",
       Arg.String (fun s -> demfile := s),
       "\tSpecify a file from which to read the initial demands."
    )
    ; ("-cport",
       Arg.Int (fun i -> cport := i),
       "\tSet the port for communicating with the controller."
    )
    ; ("-dport",
       Arg.Int (fun i -> dport := i),
       "\tSet the port for receiving bandwidth demands."
    )
  ]

let string_to_ips s =
  Str.split (Str.regexp ",") s

let empty_demands len =
  let ls = ref [] in
  for i = 0 to len do
    ls := 1L :: !ls
  done;
  !ls

let read_demands polfile =
  let contents = Merlin_Util.load_file polfile in
  let lines = Str.split (Str.regexp ";") contents in
  let non_empty = List.filter (fun line -> not (String.trim line = "")) lines in
  List.map (fun line ->
    let word = String.trim line in
    Int64.of_string word
  ) non_empty


let usage =
  Printf.sprintf "usage: %s [OPTIONS] <port>\n" Sys.argv.(0)

let _ =
  Arg.parse arg_spec (fun p -> cport := int_of_string p) usage;
  match !runmode with
    | Server ->
      let demands = if !demfile = ""
        then empty_demands !len
        else read_demands !demfile
      in

      let pol = if !polfile = ""
        then !policy
        else Merlin_FrontEnd.parse_policy_file !polfile
      in
      Printf.printf "Using policy: \n%s\n%!" (Merlin_Pretty.string_of_program_ast pol);
      policy := pol;

      let module C = struct
        let cport = !cport
        let dport = !dport
        let max = max
        let min = min
        let demands = ref demands
        let policy = policy
      end in
      let module S = Sharer.Server(C) in

      let ctrlr, demander = S.start () in
      Thread.join ctrlr;
      Thread.join demander
    | IRServer ->
      Lwt_main.run (
      let demands = Hashtbl.create 10 in
      let policy = Merlin_FrontEnd.parse_ir_file !polfile in
      let module C = struct
        let cport = !cport
        let dport = !dport
        let max = max
        let min = min
        let demands = demands
        let policy = ref policy
      end in
      let module S = Localizer.Server(C) in
      let ctrlr, demander, updater = S.start () in
      Lwt.join [ctrlr; demander; updater]
      )
    | _ ->
      Printf.printf "Don't know what to do."
