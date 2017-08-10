open Frenetic_Network
open Merlin_Util
open Merlin_Types

module VertexHash = Frenetic_Network.Net.Topology.VertexHash

(* module CodeHash = Hashtbl.Make(struct *)
(*   type t = code *)
(*   let hash = Hashtbl.hash *)
(*   let equal = (=) *)
(* end) *)

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

let nodes_to_paths : vertex list NPH.t = NPH.create 17

let symbol_table : LocationSet.t StringHash.t = StringHash.create 17

let location_to_node : vertex LocationHash.t = LocationHash.create 17

let location_to_symbol : symbol LocationHash.t = LocationHash.create 17

let symbol_to_location : location SymbolHash.t = SymbolHash.create 17

let location_to_addr : addr LocationHash.t = LocationHash.create 17

let addr_to_location : location AddrHash.t = AddrHash.create 17

let location_to_functions : StringSet.t LocationHash.t = LocationHash.create 17

let function_to_locations : LocationSet.t StringHash.t = StringHash.create 17

let stmt_to_externals : (StringSet.t * StringSet.t) StringHash.t = StringHash.create 17

let var_to_rate : (rate_option) StringHash.t = StringHash.create 17

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
  LocationHash.add location_to_symbol s n;
  SymbolHash.add symbol_to_location n s;
  n

let end_symbol : symbol =
  let n = next_symbol () in
  let s = "_out_" in
  LocationHash.add location_to_symbol s n;
  SymbolHash.add symbol_to_location n s;
  n

let get_symbol loc =
  try LocationHash.find location_to_symbol loc
  with Not_found ->
    let sym = next_symbol () in
    LocationHash.add location_to_symbol loc sym;
    SymbolHash.add symbol_to_location sym loc;
    sym

let symbol_to_string (s:symbol) : string =
  let loc = try SymbolHash.find symbol_to_location s
    with Not_found -> "Unlocated" in
  Printf.sprintf "%s(%d)" loc s

let name_to_node (name:string) =
  LocationHash.find location_to_node name

