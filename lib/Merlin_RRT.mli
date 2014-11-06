open Merlin_Types

type tree = {
  topology : topo
  ; probability : float
  ; edges_to_paths : (edge, edge list) Hashtbl.t
  ; endpoint_to_vertex : vertex VertexHash.t
  ; vertex_to_vertex : vertex VertexHash.t
}

module TreeSet : Set.S with type elt = tree

val constrain  : topo -> TreeSet.t
val pathfinder : ast_policy -> topo -> TreeSet.t -> flow list
val codegen    : flow list -> instruction list
