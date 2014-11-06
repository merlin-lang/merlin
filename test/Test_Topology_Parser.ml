open OUnit
open Unix
open Merlin_FrontEnd

let test_topology_parser f =
  let _ = parse_topo_file f in true
                            
TEST "./examples/foreach/foreach.dot" = test_topology_parser "./examples/foreach/foreach.dot" = true
TEST "./examples/hadoop/hadoop.dot" = test_topology_parser "./examples/hadoop/hadoop.dot" = true
TEST "./examples/simple/simple.dot" = test_topology_parser "./examples/simple/simple.dot" = true
TEST "./examples/sleuth/sleuth.dot" = test_topology_parser "./examples/sleuth/sleuth.dot" = true
TEST "./examples/function/function.dot" = test_topology_parser "./examples/function/function.dot" = true
TEST "./examples/order/order.dot" = test_topology_parser "./examples/order/order.dot" = true
TEST "./examples/speed/speed.dot" = test_topology_parser "./examples/speed/speed.dot" = true
TEST "./examples/rateless/rateless.dot" = test_topology_parser "./examples/rateless/rateless.dot" = true
TEST "./examples/max/max.dot" = test_topology_parser "./examples/max/max.dot" = true
TEST "./examples/dpi_start/dpi_start.dot" = test_topology_parser "./examples/dpi_start/dpi_start.dot" = true
TEST "./examples/dpi_end/dpi_end.dot" = test_topology_parser "./examples/dpi_end/dpi_end.dot" = true
TEST "./examples/inline-set/inline-set.dot" = test_topology_parser "./examples/inline-set/inline-set.dot" = true
