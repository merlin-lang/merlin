(* Gurobi LP file format *)

open Buffer
open Merlin_LPTypes
open Merlin_Util

let rec buffer_of_linear_expr e buf =
  match e with
  | Var(s) -> add_string buf s
  | Float(f) -> add_string buf (Printf.sprintf "%.1f" f)
  | Int(i)-> add_string buf (Int64.to_string i)
  | Plus(lhs,rhs) ->
    buffer_of_linear_expr lhs buf;
    add_string buf " + ";
    buffer_of_linear_expr rhs buf
  | Minus(lhs,rhs) ->
    buffer_of_linear_expr lhs buf;
    add_string buf " - ";
    buffer_of_linear_expr rhs buf
  | Mult(lhs,rhs) ->
    buffer_of_linear_expr lhs buf;
    add_string buf " ";
    buffer_of_linear_expr rhs buf
  | Div(lhs,rhs) ->
    buffer_of_linear_expr lhs buf;
    add_string buf " / ";
    buffer_of_linear_expr rhs buf

let buffer_of_relop o buf =
 match o with
 | Eq -> add_string buf "="
 | Leq -> add_string buf "<="
 | Geq -> add_string buf ">="

let buffer_of_const c buf =
  match c with
  | Constraint(x,y,z) ->
    buffer_of_linear_expr x buf;
    add_string buf "\n";
    buffer_of_relop y buf;
    add_string buf " ";
    buffer_of_linear_expr z buf

let buffer_of_bound c buf =
  match c with
  | Bound(x,y,z) ->
    buffer_of_linear_expr x buf;
    add_string buf " ";
    buffer_of_relop y buf;
    add_string buf " ";
    buffer_of_linear_expr z buf

let buffer_of_id_pw ls buf =
  List.iter (fun l -> add_string buf l ; add_string buf " ") ls

let buffer_of_const_pn ls buf =
  List.iter (fun l -> buffer_of_const l buf; add_string buf "\n") ls

let buffer_of_bound_pn bs buf =
  List.iter (fun l -> buffer_of_bound l buf; add_string buf "\n") bs

let buffer_of_sos s buf =
  match s with
  | Sos(str) -> add_string buf str
  | NoSos -> ()

let buffer_of_type_decl t buf =
  match t with
  | Binary(l) ->
    add_string buf "Binary\n";
    buffer_of_id_pw l buf
  | Integers(l) ->
    add_string buf "Integers\n    %s";
    buffer_of_id_pw l buf
  | Semis(l) ->
    add_string buf "Semis\n    %s";
    buffer_of_id_pw l buf
  | Generals(l) ->
    add_string buf "Generals\n    %s";
    buffer_of_id_pw l buf

let buffer_of_types ts buf =
  List.iter (fun t -> (buffer_of_type_decl t buf); add_string buf "\n") ts

let buffer_of_bounds b buf =
  add_string buf "Bounds\n";
  buffer_of_bound_pn b buf

let buffer_of_constraints l buf =
  add_string buf "Subject To\n";
  (buffer_of_const_pn l buf)

let buffer_of_objective o buf =
  match o with
    | Maximize(ls) ->
      add_string buf "Maximize\n";
      List.iter (fun l -> buffer_of_linear_expr l buf ; add_string buf "\n") ls
    | Minimize(ls) ->
      add_string buf "Minimize\n";
      List.iter (fun l -> buffer_of_linear_expr l buf ; add_string buf "\n") ls

let buffer_of_lp lp =
  let buf = create 1000 in
  match lp with
    | LP(o, c, b, t, s) ->
      buffer_of_objective o buf;
      buffer_of_constraints c buf;
      buffer_of_bounds b buf;
      buffer_of_types t buf;
      add_string buf "\nEnd";
      contents buf
