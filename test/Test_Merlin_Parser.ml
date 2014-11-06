open OUnit
open Merlin_FrontEnd

let test_merlin_parser f =
  let _ =  parse_policy_file f in true

TEST "./examples/foreach/foreach.mln" = test_merlin_parser "./examples/foreach/foreach.mln" = true
TEST "./examples/hadoop/hadoop.mln" = test_merlin_parser "./examples/hadoop/hadoop.mln" = true
TEST "./examples/simple/simple.mln" = test_merlin_parser "./examples/simple/simple.mln" = true
TEST "./examples/sleuth/sleuth.mln" = test_merlin_parser "./examples/sleuth/sleuth.mln" = true
TEST "./examples/function/function.mln" = test_merlin_parser "./examples/function/function.mln" = true
TEST "./examples/mmfs/mmfs.mln" = test_merlin_parser "./examples/mmfs/mmfs.mln" = true
TEST "./examples/order/order.mln" = test_merlin_parser "./examples/order/order.mln" = true
TEST "./examples/speed/speed.mln" = test_merlin_parser "./examples/speed/speed.mln" = true
TEST "./examples/rateless/rateless.mln" = test_merlin_parser "./examples/rateless/rateless.mln" = true
TEST "./examples/max/max.mln" = test_merlin_parser "./examples/max/max.mln" = true
TEST "./examples/dpi_start/dpi_start.mln" = test_merlin_parser "./examples/dpi_start/dpi_start.mln" = true
TEST "./examples/dpi_end/dpi_end.mln" = test_merlin_parser "./examples/dpi_end/dpi_end.mln" = true
TEST "./examples/inline-set/inline-set.mln" = test_merlin_parser "./examples/inline-set/inline-set.mln" = true
