open Merlin_lang.Merlin_FrontEnd

let file fn f () =
  let catch f =
    try let _ = fn f in true
    with _ -> false in
  Alcotest.(check bool) f true (catch f)

let topology f = file parse_topo_file f

let policy f = file parse_program_file f
