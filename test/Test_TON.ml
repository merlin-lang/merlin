open Core
open Merlin_lang.Merlin_FrontEnd

let names = ["did"; "ip-multicast"; "isolation"; "mapreduce"; "transforms"]
let tests example_dir =
  let tests = List.map ~f:(fun name ->
      let file = Filename.of_parts [example_dir; "ton"; name ^ ".mln"] in
      let test = Test_Parser.policy file in
      name, `Quick, test
    ) names in
  [ ( "Transactions on Networking", tests ) ]
