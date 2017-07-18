(* Write tests in independent modules, then run them here *)
open Core

let home =
  match Sys.getenv "MERLIN_HOME" with
  | Some s -> s
  | None -> match Sys.getenv "HOME" with
    | Some s -> s ^ "/src/merlin/"
    | None -> failwith "Cannot find Merlin directory"

let path name ext =
  Filename.of_parts [ home ; "examples"; name ; name ^ ext ]

let test_file ?(speed=`Quick) fn ext name =
  let path = path name ext in
  (name, speed, fn path)

let test_example ?(speed=`Slow) fn name =
  let policy = path name ".mln" in
  let topology = path name ".dot" in
  let expected = path name ".exp" in
  List.map ( fn topology policy expected )
    ~f:(fun (n,t) ->
        let name' = String.concat ~sep:" " [name; n] in
        ( name', speed, t))

let examples = ["min"; "max"; "defense"]

let () =
  let policy_parsing = List.map examples
      ~f:(test_file Test_Parser.policy ".mln") in
  let topology_parsing = List.map examples
      ~f:(test_file Test_Parser.topology ".dot") in
  let solving = List.join
      (List.map examples ~f:(test_example Test_Solver.solve)) in

 Alcotest.run "Parsers"
    [ "Policy parser" , policy_parsing ;
      "Topology parser", topology_parsing;
      "Solver", solving ]
