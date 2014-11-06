open Merlin_Error

let to_click (fn:string) =
  match fn with
    | "dpi" ->
      "FromDevice(eth1) -> PacketInspector(xx) -> Queue -> ToDevice(eth1);"
    | "ids" ->
      "FromDevice(eth1) -> Queue -> ToDevice(eth1);"
    | "fire1" ->
      "FromDevice(eth1) -> Queue -> ToDevice(eth1);"
    | "fire2" ->
      "FromDevice(eth1) -> Queue -> ToDevice(eth1);"
    | "compress" ->
      "FromDevice(eth1) -> Queue -> ToDevice(eth1);"
    | s -> raise (Unknown_click_function s)
