open Merlin_Types
open Merlin_Util
open Merlin_Error
open Frenetic_OpenFlow
open Frenetic_Packet

module NK = Frenetic_NetKAT

let major = ref 0

let fresh_major () : int =
  incr major ; !major


module TC = struct
type tcdev = string

type tchandle = int * int

type tcrate = rate
type tcprio = TCPrio of int

type tcqdisc = TCQdisc of tchandle option * tchandle * int option

type tcclass = TCClass of tchandle * tchandle * tcrate  * tcprio

type tcfilter = Merlin_Types.pred

type tccmd =
  | TCQdiscCmd of tcdev * tcqdisc
  | TCClassCmd of tcdev * tcclass
  | TCFilterCmd of tcdev * tchandle * tcfilter * tchandle

(* Utility functions *)
let get_byte (n:int64) (i:int) : int =
  if i < 0 || i > 5 then
    raise (Invalid_argument "Int64.get_byte index out of range");
  Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical n (8 * i)))

let string32_of_dlAddr x =
  let fst = Format.sprintf "0x%02x%02x%02x%02x"
    (get_byte x 5) (get_byte x 4) (get_byte x 3) (get_byte x 2) in
  let snd = Format.sprintf "0x%02x:%02x0000" (get_byte x 1) (get_byte x 0) in
  (fst,snd)

let rate_to_mbps_string n =
  let mbps = Int64.div n 131072L in
  Printf.sprintf "%Ld mbps" mbps

(* Convert TC types to command strings *)
let string_of_tchandle (h1,h2) =
  Printf.sprintf "%d:%d" h1 h2

let string_of_tcqdisc qd =
  match qd with TCQdisc(p, c, d) ->
    let parent = match p with
      | None -> "root"
      | Some((h1,h2)) -> Printf.sprintf "%d:%d" h1 h2 in
    let handle, _ = c in                (* Qdisc handle only has a major number *)
    let default = match d with
      | None -> ""
      | Some(i) -> Printf.sprintf "default %d" i in
    Printf.sprintf "parent %s handle %d: htb %s" parent handle default

let string_of_tcclass c =
  match c with TCClass(p, i, r, pr) ->
    let (p1,p2) = p in
    let (i1,i2) = i in
    let Rate(min,max) = r in
    let rate = rate_to_mbps_string min in
    let r = if rate = "" then rate else "rate " ^ rate in
    let ceiling = rate_to_mbps_string max in
    let c = if ceiling = "" then ceiling else "ceil " ^ ceiling in
    let TCPrio(p) = pr in
    Printf.sprintf "parent %d:%d classid %d:%d %s %s prio %d"
      p1 p2 i1 i2 r c p

let string_of_test t =  
  match t with
    | NK.EthSrc n -> 
      let fst,snd = string32_of_dlAddr n in 
      Printf.sprintf
        "protocol 802_3 u32 match u32 %s 0xffffffff at 6 match u32 %s 0xffff0000 at 10"
        fst snd
    | NK.EthDst n -> 
      let fst,snd = string32_of_dlAddr n in 
      Printf.sprintf
        "protocol 802_3 u32 match u32 %s 0xffffffff at 0 match u32 %s 0xffff0000 at 4"
        fst snd
    | NK.IP4Src (n,_) -> 
      Printf.sprintf "protocol ip prio 1 u32 match ip src %s"
        (string_of_nwAddr n)
    | NK.IP4Dst (n,_) -> 
      Printf.sprintf "protocol ip prio 1 u32 match ip dst %s"
        (string_of_nwAddr n)
    | NK.IPProto n -> 
      Printf.sprintf "protocol ip prio 1 u32 match ip protocol %s"
        (string_of_int n)
    | NK.TCPSrcPort n -> 
      Printf.sprintf "protocol ip prio 1 u32 match ip sport %s"
        (string_of_int n)
    | NK.TCPDstPort n -> 
      Printf.sprintf "protocol ip prio 1 u32 match ip dport %s"
        (string_of_int n)
    | _ -> 
      ""(* raise (Unimplementable_tc_test f) *)

let rec string_of_tcfilter (p:tcfilter) : string = match p with
  | Test (t) -> string_of_test t
  | And (p,p') -> 
    Printf.sprintf "%s %s"
      (string_of_tcfilter p)
      (string_of_tcfilter p')
  (* This is a hack. tc really needs separate filters attached to the same class
     to implement an or. So whatever's calling this function should ideally pull
     the Or apart and issue separate calls. The \n is to allow the string to
     be split after-the-fact, though that should not be relied on *)
  | Or (p, p') -> Printf.sprintf "%s\n%s"
                                 (string_of_tcfilter p)
                                 (string_of_tcfilter p')
  | Nothing -> ""
  | Everything -> ""
  | p -> raise (Unimplementable_tc_predicate p)

(* Only support adding new control settings for now *)
let string_of_tccmd cmd =
  match cmd with
    | TCQdiscCmd(dev, qd) -> Printf.sprintf "tc qdisc add dev %s %s" dev
      (string_of_tcqdisc qd)
    | TCClassCmd(dev, cl) -> Printf.sprintf "tc class add dev %s %s" dev
      (string_of_tcclass cl)
    | TCFilterCmd(dev, parent, pr, h) ->
      Printf.sprintf "tc filter add dev %s parent %s %s flowid %s" dev
      (string_of_tchandle parent)
      (string_of_tcfilter pr)
      (string_of_tchandle h)

let tccmds_of_spec (pr:tcfilter) (r:tcrate) =
  let dev = "eth0" in
  let prio = TCPrio(1) in
  let major = (fresh_major ()) in
  let qhandle = (major,0) in
  let chandle = (major, 1) in
  let qd = TCQdisc(None, qhandle, None) in
  let cl = TCClass(qhandle,chandle,r,prio) in
  [TCQdiscCmd(dev, qd) ; TCClassCmd(dev, cl) ; TCFilterCmd(dev, qhandle, pr, chandle)]
end
