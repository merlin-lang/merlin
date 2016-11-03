open Merlin_NFA
open Merlin_FrontEnd

let%test "any_path" =
    let r = parse_regex_string "h1 .* h2" in
    is_any_path r
