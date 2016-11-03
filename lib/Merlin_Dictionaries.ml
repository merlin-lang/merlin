open Frenetic_Network
open Merlin_Util
open Merlin_Types

module VertexHash = Frenetic_Network.Net.Topology.VertexHash

module CodeHash = Hashtbl.Make(struct
  type t = code
  let hash = Hashtbl.hash
  let equal = (=)
end)

module SymbolSet = Set.Make(struct
  type t = symbol
  let compare = Pervasives.compare
end)

module SymbolHash = Hashtbl.Make(struct
  type t = symbol
  let hash = Hashtbl.hash
  let equal = (=)
end)

module AddrHash = Hashtbl.Make(struct
  type t = addr
  let hash = Hashtbl.hash
  let equal = (=)
end)

module IntHash = Hashtbl.Make(struct
  type t = int
  let hash = Hashtbl.hash
  let equal = (=)
end)

module Int64Hash = Hashtbl.Make(struct
  type t = int64
  let hash = Hashtbl.hash
  let equal = (=)
end)

module NPH = NodePairHash

let print_paths t =
  let open NPH in
  Printf.printf "-------------------\n";
  NodePairHash.iter (fun key value -> Printf.printf "hi\n" ) t;
  Printf.printf "-------------------\n"

(* let get_node_path t ns = *)
(*   let open NPH in *)
(*   let h = Hashtbl.hash ns in *)
(*   NodePairHash.find t {hash = Some h; data = ns} *)

(* let set_node_path t ns p = *)
(*   let open NPH in *)
(*   let h = Hashtbl.hash ns in *)
(*   NodePairHash.replace t {hash = Some h; data = ns} p *)

(* let node_path_mem t ns = *)
(*   let open NPH in *)
(*   let h = Hashtbl.hash ns in *)
(*   NodePairHash.mem t {hash = Some h; data = ns} *)

let nodes_to_paths : vertex list NPH.t = NPH.create 17

let symbol_to_code : code SymbolHash.t = SymbolHash.create 17

let code_to_symbol : symbol CodeHash.t = CodeHash.create 17

let location_to_node : vertex LocationHash.t = LocationHash.create 17

let location_to_symbols : SymbolSet.t LocationHash.t = LocationHash.create 17

let location_to_addr : addr LocationHash.t = LocationHash.create 17

let addr_to_location : location AddrHash.t = AddrHash.create 17

let symbol_table : LocationSet.t StringHash.t = StringHash.create 17

let stmt_to_externals : (StringSet.t * StringSet.t) StringHash.t = StringHash.create 17

let add_externals stmt src dst =
  let srcs,dsts = try StringHash.find stmt_to_externals stmt with Not_found -> (StringSet.empty, StringSet.empty) in
  StringHash.replace stmt_to_externals stmt ((StringSet.add src srcs), (StringSet.add dst dsts))

let symbol_cell = ref (-1)

let next_symbol () : symbol =
  incr symbol_cell;
  !symbol_cell

(* TODO(jnf): make code in Merlin_Topology use these definitions *)
let start_symbol : symbol =
  let n = next_symbol () in
  let s = "_in_" in
  SymbolHash.add symbol_to_code n (s, None);
  CodeHash.add code_to_symbol (s, None) n;
  LocationHash.add location_to_symbols s (SymbolSet.singleton n);
  n

let end_symbol : symbol =
  let n = next_symbol () in
  let s = "_out_" in
  SymbolHash.add symbol_to_code n (s, None);
  CodeHash.add code_to_symbol (s, None) n;
  LocationHash.add location_to_symbols s (SymbolSet.singleton n);
  n

let get_symbol c =
  if CodeHash.mem code_to_symbol c then
    CodeHash.find code_to_symbol c
  else
    begin
      let n = next_symbol () in
      let (l,_) = c in
      let ns = try LocationHash.find location_to_symbols l with Not_found -> SymbolSet.empty in
      SymbolHash.add symbol_to_code n c;
      CodeHash.add code_to_symbol c n;
      LocationHash.add location_to_symbols l (SymbolSet.add n ns);
      n
    end

let symbol_to_string (s:symbol) : string =
  try
    let l,fo = SymbolHash.find symbol_to_code s in
    match fo with
      | None -> if l = "_in_" || l = "_out_"  then "" else l
      | Some f -> f
  with Not_found -> failwith (Printf.sprintf "Undefined symbol: %d " s)


let name_to_node (name:string) =
  LocationHash.find location_to_node name
