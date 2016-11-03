open Merlin_FrontEnd

let test_queue input =
  let t = Printf.sprintf "./examples/%s/%s.dot" input input in
  let p =  Printf.sprintf "./examples/%s/%s.mln" input input in

  let topo = parse_topo_file t in
  let ir =   policy_file_to_ir p in
  let flows = match ir with
    |Some ir -> solve ir topo
    | None -> [] in
  let (_, qcs, _, _) = Merlin_Generate.from_flows topo flows in

  (* TODO: this test will only check that there should be
    one queue for the min.mln example. We need something
    more extensible. *)
  (List.length qcs) == 1


(* (\* These are expected to pass *\) *)
let%test "./examples/min/min.mln" = test_queue "min" = true
