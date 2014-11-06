open OUnit
open Merlin_NFA
open Merlin_FrontEnd

TEST "any_path" =
    let r = parse_regex_string "h1 .* h2" in
    is_any_path r
