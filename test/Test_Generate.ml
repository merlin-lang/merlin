open OUnit
open SDN_Types
open Merlin_Types
open Merlin_Generate

TEST "reminder to write a test" =
  false

(*
let is_vlan action = match action with
  | SetField (f,v) -> begin match f with
      | Vlan -> true
      | _ -> false end
  | _ -> false

let set_vlan action value = match action with
  | SetField (f,v) -> begin match f with
      | Vlan -> v = value
      | _ -> false end
  | _ -> false

TEST "openflow queueless forward" =
    let ip = Test (SDN_Types.IP4Src, VInt.Int32 (Int32.of_int 111)) in
    let path = [(VInt.Int32 1l, VInt.Int32 0l, VInt.Int32 2l,
                 None,IngressEgress)] in
    let ofs,_ = Flow.to_switch_conf (ip,path) in
    let _,acts,_ = List.hd ofs in
    let out = List.hd acts in
    out = OutputPort (VInt.Int32 2l) && List.length ofs = 1


TEST "openflow queueless tag untag" =
    let ip = Test (SDN_Types.IP4Src, VInt.Int32 (Int32.of_int 111)) in
    let path = [(VInt.Int32 1l, VInt.Int32 0l, VInt.Int32 2l,None,Ingress)
               ;(VInt.Int32 1l, VInt.Int32 1l, VInt.Int32 2l,None,Egress)] in
    let ofs,_ = Flow.to_switch_conf (ip, path) in
    let _,tag_a,tag_s = List.hd ofs in
    let _,untag_a,untag_s = List.nth ofs 1 in

    let tag_out = List.nth tag_a 1 in
    let untag_out = List.nth untag_a 1 in

    let tag = ( is_vlan (List.hd tag_a)
                && tag_out = OutputPort (VInt.Int32 2l)
                && tag_s = VInt.Int32 0l) in

    let untag = ( set_vlan (List.hd untag_a) (VInt.Int16 0xffff)
                && untag_out = OutputPort (VInt.Int32 2l)
                && untag_s = VInt.Int32 1l) in

    tag && untag && List.length ofs = 2

TEST "openflow queueless tag fwd untag" =
    let ip = Test (SDN_Types.IP4Src, VInt.Int32 (Int32.of_int 111)) in
    let path = [(VInt.Int32 1l, VInt.Int32 0l, VInt.Int32 2l,None,Ingress)
               ;(VInt.Int32 1l, VInt.Int32 1l, VInt.Int32 2l,None,Intermediate)
               ;(VInt.Int32 1l, VInt.Int32 2l, VInt.Int32 2l,None,Egress)] in
    let ofs,_ = Flow.to_switch_conf (ip, path) in
    let _,tag_a,tag_s = List.hd ofs in
    let _,fwd_a,fwd_s = List.nth ofs 1 in
    let _,untag_a,untag_s = List.nth ofs 2 in

    let tag_out = List.nth tag_a 1 in
    let fwd_out = List.hd fwd_a in
    let untag_out = List.nth untag_a 1 in


    let tag = ( is_vlan (List.hd tag_a)
                 && tag_out = OutputPort (VInt.Int32 2l)
                 && tag_s = VInt.Int32 0l) in

    let fwd = ( fwd_out = OutputPort (VInt.Int32 2l)
                && fwd_s = VInt.Int32 1l
                && List.length fwd_a = 1) in

    let untag = ( set_vlan (List.hd untag_a) (VInt.Int16 0xffff)
                && untag_out = OutputPort (VInt.Int32 2l)
                && untag_s = VInt.Int32 2l) in

    tag && fwd && untag && List.length ofs = 3
*)
