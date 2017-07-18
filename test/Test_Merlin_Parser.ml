open Merlin_lang.Merlin_FrontEnd

let test_merlin_parser f () =
  let catch f =
    try
      let _ = parse_policy_file f in true
    with _ -> false in
  Alcotest.(check bool) f true (catch f)

let run () =
  let home = try Sys.getenv "MERLIN_HOME"
    with Not_found -> ( Sys.getenv "HOME" ) ^ "/src/merlin/" in

  let test name =
    let path = Core.Filename.of_parts [ home ; "examples"; name ; name ^ ".mln"] |>
               Core.Filename.realpath in
    ( name, `Quick, test_merlin_parser path ) in

  let tests = [ test "min"; test "max" ] in
  Alcotest.run "Merlin Parser tests" [ "Merlin_Parser", tests ]
