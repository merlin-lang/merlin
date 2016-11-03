open Merlin_FrontEnd

let test_merlin_parser f =
  let _ =  parse_policy_file f in true

let%test "./examples/foreach/foreach.mln" = test_merlin_parser "./examples/foreach/foreach.mln" = true
let%test "./examples/hadoop/hadoop.mln" = test_merlin_parser "./examples/hadoop/hadoop.mln" = true
let%test "./examples/simple/simple.mln" = test_merlin_parser "./examples/simple/simple.mln" = true
let%test "./examples/sleuth/sleuth.mln" = test_merlin_parser "./examples/sleuth/sleuth.mln" = true
let%test "./examples/function/function.mln" = test_merlin_parser "./examples/function/function.mln" = true
let%test "./examples/mmfs/mmfs.mln" = test_merlin_parser "./examples/mmfs/mmfs.mln" = true
let%test "./examples/order/order.mln" = test_merlin_parser "./examples/order/order.mln" = true
let%test "./examples/speed/speed.mln" = test_merlin_parser "./examples/speed/speed.mln" = true
let%test "./examples/rateless/rateless.mln" = test_merlin_parser "./examples/rateless/rateless.mln" = true
let%test "./examples/max/max.mln" = test_merlin_parser "./examples/max/max.mln" = true
let%test "./examples/dpi_start/dpi_start.mln" = test_merlin_parser "./examples/dpi_start/dpi_start.mln" = true
let%test "./examples/dpi_end/dpi_end.mln" = test_merlin_parser "./examples/dpi_end/dpi_end.mln" = true
let%test "./examples/inline-set/inline-set.mln" = test_merlin_parser "./examples/inline-set/inline-set.mln" = true
