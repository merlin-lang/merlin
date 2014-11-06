open OUnit
open Merlin_FrontEnd

let test_click input expected =

 let t = Printf.sprintf "./examples/%s/%s.dot" input input in
  let p =  Printf.sprintf "./examples/%s/%s.mln" input input in

  let topo = parse_topo_file t in
  let ir =   policy_file_to_ir p in
  let flows = match ir with
    |Some ir -> solve ir topo
    | None -> [] in
  let (_, _, _, clicks) = Merlin_Generate.from_flows topo flows in

  (* TODO: this test will only check that there should be
    the expected number click command for the input. We need something
     more extensible. *)
  (List.length clicks) == expected


(* (\* These are expected to pass *\) *)
TEST "./examples/dpi_start/dpi_start.mln" = test_click "dpi_start" 1 = true
TEST "./examples/dpi_end/dpi_end.mln" = test_click "dpi_end" 1 = true
TEST "./examples/function/function.mln" = test_click "function" 1 = true
