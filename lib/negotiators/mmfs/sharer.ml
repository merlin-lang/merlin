open Unix
open Merlin_Util
open Merlin_Types
open Merlin_Pretty
open Merlin_Error

type demtbl = (string, Int64.t) Hashtbl.t

module type Conf = sig
  val min : Int64.t ref
  val max : Int64.t ref
  val demands : Int64.t list ref
  val cport : int
  val dport : int
  val policy : ast_program ref
end

module Server(C:Conf) = struct

  let negpol = ref (Merlin_Types.ASTProgram [])
  let mutex = Mutex.create ()
  let ctrlr = ref Unix.stdout

  let share_by_flow (demands: (ast_policy * Int64.t) list) (min:Int64.t) (max:Int64.t) =
      let sorted_demands = List.sort (fun (_,d1) (_,d2) ->
        Int64.to_int (Int64.sub d2 d1)
      ) demands in
      let num_pol = List.length demands in
      let fairmin = Int64.div min (Int64.of_int num_pol) in
      let rem,_,newpols = List.fold_left
        (fun (remmax,left,newpols) (pol,demand) ->
          let ASTPolicy(q,pred,rx,rate) = pol in

          let polmin,polmax = match rate with
            | RMin(n) -> n, Int64.max_int
            | RMax(n) -> 1L, n
            | RNone -> 1L, Int64.max_int
            | RBoth(min,max) -> min,max in
          let fairmax = Int64.div remmax (Int64.of_int left) in
          let newmin = if fairmin < polmin then fairmin else polmin in
          let newmax = if fairmax < demand then fairmax else demand in
          let newpol = ASTPolicy(q,pred,rx,RBoth(newmin,newmax)) in
          (Int64.sub remmax newmax, left-1, newpol::newpols)
        )  (max,num_pol,[]) sorted_demands in
      let stmts = List.rev newpols in

      (* Share any remaining bandwidth *)
      (* TODO: Share remaining bandwidth only between hosts with max caps *)
      let share = Int64.div rem (Int64.of_int num_pol) in
      let stmts' = List.map (fun stmt ->
        let ASTPolicy(q,pred,rx,rate) = stmt in
        match rate with
          | RMin(n) -> raise Not_Implemented
          | RMax(n) -> raise Not_Implemented
          | RNone  -> raise Not_Implemented
          | RBoth(min,max)->
            let incmax = Int64.add max share in
            Printf.printf "New min %Ld and max:%Ld\n%!" min incmax;
            ASTPolicy(q,pred,rx,RBoth(min,incmax))
      ) stmts in
      stmts'

  let negotiate (p:ast_program) (d:Int64.t list) (min:Int64.t) (max:Int64.t) =
    let ASTProgram(ss) = p in
    let shared_pol = share_by_flow (List.combine ss d) min max in
    let pgm = ASTProgram(shared_pol) in
    pgm

  let inform_ctrlr (p:ast_program) =
    send_all !ctrlr (string_of_program_ast p)

  let rec update_demands socket =
    let rec loop socket =
      let msg = String.trim (recv_all socket) in
      Printf.printf "Received message: %s\n%!" msg;
      if msg = "DONE" then
        false
      else
        let lines = Str.split (Str.regexp ";") msg in
        Printf.printf "Number of lines %d\n" (List.length lines) ;
        let non_empty =
          List.filter (fun line -> not (String.trim line = "")) lines in
        let demands = List.map (fun line ->
          let word = String.trim line in
          Int64.of_string word
        ) non_empty in

        Mutex.lock mutex;
        C.demands := demands;
        (* List.iter (fun p -> *)
        (*   let demand = Str.bounded_split (Str.regexp " ") (String.trim p) 3 in *)
        (*   let ip = List.hd demand in *)
        (*   let amt = Int64.of_string (List.nth demand 1) in *)
        (*   Hashtbl.replace C.demands ip amt *)
        (* ) pairs; *)
        let negotiated = negotiate !C.policy !C.demands !C.min !C.max in
        let _ = inform_ctrlr negotiated in
        Mutex.unlock mutex;
        loop socket
    in
    send_all socket "READY to receive policies\n";
    loop socket

  let update_policy socket =
    let rec loop socket =
      let msg = String.trim (recv_all socket) in
      if msg = "DONE" then
        false
      else
        let pol = Merlin_FrontEnd.parse_policy_string msg in
        Mutex.lock mutex;
        C.policy := pol;
        let negotiated = negotiate !C.policy !C.demands !C.min !C.max in
        let _ = inform_ctrlr negotiated in
        Mutex.unlock mutex;
        loop socket
    in
    Mutex.lock mutex;
    ctrlr := socket;
    send_all socket "READY to receive policies\n";
    Mutex.unlock mutex;
    loop socket


  (* Network connection Functions *)
  let rec accept_loop socket handler =
    let (cl_socket, cl_addr) = accept socket in
    Printf.printf "Accepted something\n";
    let repeat = handler cl_socket in
    if repeat then accept_loop socket handler
    else ()

  let serve (port, handler, name) =
    let socket = local_bind_to port in
    listen socket 10;
    Printf.printf "Listening for %s on port: %d\n%!" name port;
    accept_loop socket handler

  let start () =
    let ctrlr = Thread.create serve (C.cport, update_policy, "ctrlr") in
    let demander = Thread.create serve (C.dport, update_demands, "demander") in
    (ctrlr,demander)

end
