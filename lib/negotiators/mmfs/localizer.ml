open Unix
open Merlin_Util
open Merlin_Types
open Merlin_Pretty
open Merlin_Error

type demand_tbl = (string, Int64.t) Hashtbl.t

type update =
  | PolicyUpdate of pol
  | DemandUpdate of demand_tbl

module type Config = sig
  val min : Int64.t ref
  val max : Int64.t ref
  val demands : demand_tbl
  val cport : int
  val dport : int
  val policy : pol ref
end


module Server(C:Config) = struct
  let ctrlr = ref Lwt_unix.stdout

  let negotiate (p:pol) (d:demand_tbl) (min:Int64.t) (max:Int64.t) =
    let Pol(stmts,_) = p in
    let demlist = Hashtbl.fold (fun k v acc ->
      (k,v)::acc) d [] in
    let sorted_demands = List.sort (fun (_,d1) (_,d2) ->
        Int64.to_int (Int64.sub d2 d1)) demlist in
    let num_stmts = List.length demlist in

    (* Split the minimum allocation evenly. *)
    let fairmin = Int64.div min (Int64.of_int num_stmts) in
    let mins = List.fold_left (fun acc (var,dem) ->
      FAnd(FMin(BVar var, fairmin), acc)) FNone sorted_demands in

    (* Allocate the maximum bandwidth according to demands *)
    let allocs = Hashtbl.create num_stmts in
    let rem,_ = List.fold_left (fun (remmax,left) (var,demand) ->
      let fairmax = Int64.div remmax (Int64.of_int left) in
      let alloc = if fairmax < demand then fairmax else demand in
      Hashtbl.replace allocs var alloc;
      (Int64.sub remmax alloc, left-1)
    ) (max,num_stmts) sorted_demands in

    (* Share any remaining bandwidth *)
    let share = Int64.div rem (Int64.of_int num_stmts) in
    Hashtbl.iter (fun var dem ->
      Hashtbl.replace allocs var (Int64.add dem share)
    ) allocs;

    let maxs = Hashtbl.fold (fun var dem formula ->
      FAnd(FMax(BVar var, dem), formula)) allocs FNone in

    Pol(stmts, FAnd(mins,maxs))

  let rec updater stream =
    match_lwt (Lwt_stream.get stream) with
      | Some update ->
        let newpol = match update with
          | PolicyUpdate(pol) -> negotiate pol C.demands !C.min !C.max
          | DemandUpdate(d) -> negotiate !C.policy d !C.min !C.max in
        Printf.printf "%s\n%!" (Merlin_Pretty.string_of_ir newpol);
        lwt _ = lwt_send_all !ctrlr (Merlin_Pretty.string_of_ir newpol) in
        updater stream
      | None -> Lwt.return ()

  let update_policy port push =
    Printf.printf "Policy updater launched\n%!";
    let rec loop socket =
      lwt str = lwt_recv_all socket in
      let msg = String.trim str in
      Printf.printf "|%s|\n%!" msg;
      if msg = "DONE" then Lwt.return ()
      else
        let ir = Merlin_FrontEnd.parse_ir_string msg in
        C.policy := ir;
        push (Some (PolicyUpdate ir));
        loop socket
    in
    let socket = lwt_local_bind_to port in
    Lwt_unix.listen socket 10;
    lwt (cl_socket, cl_addr) = Lwt_unix.accept socket in
    lwt _ = lwt_send_all cl_socket "Connected\n" in
    ctrlr := cl_socket;
    loop cl_socket

  let update_demands port push =
    Printf.printf "Demand updater launched\n%!";
    let rec loop socket =
      lwt str = lwt_recv_all socket in
      let msg = String.trim str in
      Printf.printf "|%s|\n%!" msg;
      if msg = "DONE" then Lwt.return ()
      else
        let entries = Str.split (Str.regexp ";") msg in
        let non_empty =
          List.filter (fun line -> not (String.trim line = "")) entries in
        List.iter (fun s ->
          let entry = String.trim s in
          let words = Str.bounded_split (Str.regexp "[ \t]+") entry 2 in
          let var = List.hd words in
          let demand = Int64.of_string (List.nth words 1) in
          Hashtbl.replace C.demands var demand
        ) non_empty;
        push (Some (DemandUpdate C.demands));
        loop socket
    in
    let socket = lwt_local_bind_to port in
    Lwt_unix.listen socket 10;
    lwt (cl_socket, cl_addr) = Lwt_unix.accept socket in
    Printf.printf "READY to receive demands\n%!";
    lwt _ = lwt_send_all cl_socket "READY to receive demands\n" in
    loop cl_socket

  let start () =
    let update_stream, update_push = Lwt_stream.create () in
    let ctrlr = update_policy C.cport update_push in
    let demander = update_demands C.dport update_push in
    let updater = updater update_stream in
    (ctrlr, demander, updater)

end
