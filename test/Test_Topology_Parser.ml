open Unix
open Merlin_FrontEnd

let test_topology_parser f =
  let _ = parse_topo_file f in true
                            
let%test "./examples/foreach/foreach.dot" = test_topology_parser "./examples/foreach/foreach.dot" = true
let%test "./examples/hadoop/hadoop.dot" = test_topology_parser "./examples/hadoop/hadoop.dot" = true
let%test "./examples/simple/simple.dot" = test_topology_parser "./examples/simple/simple.dot" = true
let%test "./examples/sleuth/sleuth.dot" = test_topology_parser "./examples/sleuth/sleuth.dot" = true
let%test "./examples/function/function.dot" = test_topology_parser "./examples/function/function.dot" = true
let%test "./examples/order/order.dot" = test_topology_parser "./examples/order/order.dot" = true
let%test "./examples/speed/speed.dot" = test_topology_parser "./examples/speed/speed.dot" = true
let%test "./examples/rateless/rateless.dot" = test_topology_parser "./examples/rateless/rateless.dot" = true
let%test "./examples/max/max.dot" = test_topology_parser "./examples/max/max.dot" = true
let%test "./examples/dpi_start/dpi_start.dot" = test_topology_parser "./examples/dpi_start/dpi_start.dot" = true
let%test "./examples/dpi_end/dpi_end.dot" = test_topology_parser "./examples/dpi_end/dpi_end.dot" = true
let%test "./examples/inline-set/inline-set.dot" = test_topology_parser "./examples/inline-set/inline-set.dot" = true
