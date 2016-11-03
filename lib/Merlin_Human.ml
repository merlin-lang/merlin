open Frenetic_Packet
open Merlin_Util
open Merlin_Types
open Merlin_Pretty

let string_of_ofs (ofls : step list) : string =
  intercalate (fun opf ->
    let (pr, acts, sw) = opf in
    let swhdr = Printf.sprintf "On switch %Ld\t%!" sw in
    let prstr = Printf.sprintf "%s -> %!" (string_of_pred pr) in
    swhdr ^ prstr ^ (intercalate SDN_Types.string_of_action "\t" acts)
  ) "\n" ofls

let string_of_qcs (qcs:swqconf list) : string =
  let tbl = Merlin_Generate.Gather.queue_configs qcs in
  Hashtbl.fold ( fun sw ptbl str ->
    let swhdr = Printf.sprintf "On switch %Ld:\n" sw in
    let swbody = Hashtbl.fold (fun p qtbl str ->
      let phdr = Printf.sprintf "\tOn port %ld:\n" p in
      let pbody = Hashtbl.fold (fun q (min,max) str ->
        let s = Printf.sprintf "\t\t%ld -> %Ld %Ld\n"
           q min max in
        str ^ s
      ) qtbl "" in str ^ phdr ^ pbody
    ) ptbl "" in str ^ swhdr ^ swbody
  ) tbl ""


let string_of_tcs (tcs : hostconf list) : string =
  let iptbl = Merlin_Generate.Gather.tcs tcs in
  let tcbuf = Hashtbl.fold (fun ip tcs buf ->
    Buffer.add_string buf "For IP address ";
    Buffer.add_string buf (string_of_nwAddr  ip);
    Buffer.add_string buf ":\n";
    List.iter (fun tc -> Buffer.add_string buf tc;
      Buffer.add_string buf "\n"; ) tcs;
    buf
  ) iptbl (Buffer.create 1000) in
  Buffer.contents tcbuf

let string_of_clicks ( clicks : hostconf list ) : string =
  let buf =
    List.fold_left (fun buf (ip, click) ->

      Buffer.add_string buf "For IP address ";
      Buffer.add_string buf (string_of_nwAddr  ip);
      Buffer.add_string buf ":\n";
      Buffer.add_string buf click;
      Buffer.add_string buf "\n";

      buf) (Buffer.create 1000) clicks in
  Buffer.contents buf

let print_all (steps:step list) (qcs:swqconf list) (tcs:hostconf list)
    (clicks:hostconf list) : unit =
  Printf.printf "\nOpenFlow rules (%d):\n\n" (List.length steps);
  Printf.printf "%s\n" (string_of_ofs steps);
  Printf.printf "\nQueue Configurations (%d):\n\n" (List.length qcs);
  Printf.printf "%s\n" (string_of_qcs qcs);
  Printf.printf "\ntc Configurations (%d):\n\n" (List.length tcs);
  Printf.printf "%s\n" (string_of_tcs tcs);
  Printf.printf "\ntc Clicks (%d):\n\n" (List.length clicks);
  Printf.printf "%s\n" (string_of_clicks clicks)
