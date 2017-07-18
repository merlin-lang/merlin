open Frenetic_Packet
open Frenetic_Network

open Merlin_lang.Merlin_FrontEnd
open Merlin_lang.Merlin_Types
open Merlin_lang.Merlin_Dictionaries


let solve t p s =
  let topo = parse_topo_file t in
  let ir =   policy_file_to_ir p in
  let flows = match ir with
    |Some ir -> solve ir topo
    | None -> [] in

  let sol = Merlin_lang.Merlin_Util.load_lines s in
  let expected = List.fold_left (fun acc line ->
    let expected_flows = Str.split (Str.regexp " ") line in
    List.fold_left (fun acc ef ->
      let data = Str.split (Str.regexp ":") ef in
      let h = List.nth data 0 in
      let t = List.nth data 1 in
      (h,(Int64.of_string t))::acc) acc expected_flows) [] sol in

  let got = List.fold_left (fun acc f ->
    let (_,forwards) = f in
    List.fold_left (fun acc fwd ->
      let node,devid = fwd.device in
      let label = Net.Topology.vertex_to_label topo fwd.topo_vertex in
      let ip = Node.ip label in
      ((string_of_ip ip), devid)::acc
    ) acc forwards
  ) [] flows in

  (* List.iter (fun (ip, port) -> *)
  (*   Printf.printf "-> %s %Ld\n" ip port ) (List.sort Pervasives.compare expected) *)
  (* Printf.printf "---------------\n"; *)
  (* List.iter (fun (ip, port) -> *)
  (*   Printf.printf "-> %s %Ld\n" ip port ) (List.sort Pervasives.compare
       got) *)

  let len = fun () ->
    Alcotest.(check int) "Solution length"
      ( List.length expected ) ( List.length got ) in

  let matches = fun () ->
    Alcotest.(check (slist (pair string int64) Pervasives.compare))
      "Solution IP and Ports"
      expected got in

  ["length", len ; "matches", matches]
