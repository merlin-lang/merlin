(* Write tests in independent modules, then run them here *)
open Core

let home =
  match Sys.getenv "MERLIN_HOME" with
  | Some s -> s
  | None -> Filename.of_parts [ Sys.home_directory () ; "src/merlin"]

let examples =
  match Sys.getenv "MERLIN_EXAMPLES" with
  | Some s -> s
  | None -> Filename.of_parts [home ; "examples"]

let path name ext =
  Filename.of_parts [ examples; name; name ^ ext ]

let of_file ?(speed=`Quick) fn ext name =
  let path = path name ext in
  (name, speed, fn path)

let of_example ?(speed=`Slow) fn name =
  let policy = path name ".mln" in
  let topology = path name ".dot" in
  let expected = path name ".exp" in
  List.map ( fn topology policy expected )
    ~f:(fun (n,t) ->
        let name' = String.concat ~sep:" " [name; n] in
        ( name', speed, t))

(* Not sure if these should be automatically pulled from the examples/ subdirectory *)
let tests = ["min"; "max"; "defense"]

let papers = Test_TON.tests examples

let () =
  let policy_parsing = List.map tests
      ~f:(of_file Test_Parser.policy ".mln") in
  let topology_parsing = List.map tests
      ~f:(of_file Test_Parser.topology ".dot") in
  (* let solving = List.join *)
  (*     (List.map tests ~f:(of_example Test_Solver.solve)) in *)

  Alcotest.run "Merlin Compiler & Runtime"
    ( ( "Policy parser" , policy_parsing )::
      ( "Topology parser", topology_parsing )::
      papers )
(* "Solver", solving *) 
