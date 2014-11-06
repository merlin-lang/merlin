(* Gurobi LP file format *)

open Merlin_LPTypes
open Merlin_Util

let rec string_of_linear_expr e  =
  match e with
  | Var(s) -> s
  | Float(f) -> Printf.sprintf "%.1f" f
  | Int(i)-> Int64.to_string i
  | Plus(lhs,rhs) -> Printf.sprintf "%s + %s" ( string_of_linear_expr lhs)  ( string_of_linear_expr rhs)
  | Minus(lhs,rhs) -> Printf.sprintf "%s - %s" ( string_of_linear_expr lhs)  ( string_of_linear_expr rhs)
  | Mult(lhs,rhs) -> Printf.sprintf "%s %s" ( string_of_linear_expr lhs)  ( string_of_linear_expr rhs)
  | Div(lhs,rhs) ->  Printf.sprintf "%s / %s" ( string_of_linear_expr lhs)  ( string_of_linear_expr rhs)

let string_of_relop o =
 match o with
 | Eq -> "="
 | Leq -> "<="
 | Geq -> ">="

let string_of_const c =
  match c with
  | Constraint(x,y,z) -> Printf.sprintf("%s\n%s %s")
    (string_of_linear_expr x)
    (string_of_relop y)
    (string_of_linear_expr z)

let string_of_bound c =
  match c with
  | Bound(x,y,z) -> Printf.sprintf("%s %s %s")
    (string_of_linear_expr x)
    (string_of_relop y)
    (string_of_linear_expr z)

let string_of_id_pw l =
  list_intercalate (fun x -> x) " " l

let string_of_const_pn l =
  list_intercalate string_of_const "\n" l

let string_of_bound_pn b =
  list_intercalate string_of_bound "\n" b

let string_of_sos s =
  match s with
  | Sos(str) -> str
  | NoSos -> ""

let string_of_type_decl t =
  match t with
  | Binary(l) -> Printf.sprintf("Binary\n%s") (string_of_id_pw l)
  | Integers(l) ->  Printf.sprintf("Integers\n    %s") (string_of_id_pw l)
  | Semis(l) ->  Printf.sprintf("Semis\n    %s") (string_of_id_pw l)
  | Generals(l) ->  Printf.sprintf("Generals\n    %s") (string_of_id_pw l)
 
let string_of_types ts =
  list_intercalate string_of_type_decl "\n" ts
      
let string_of_bounds b =  Printf.sprintf("Bounds\n%s")  (string_of_bound_pn b)

let string_of_constraints l =  Printf.sprintf("Subject To\n%s")  (string_of_const_pn l)

let string_of_objective o =
  match o with
  | Maximize(l) -> Printf.sprintf("Maximize\n%s") ( list_intercalate string_of_linear_expr "\n" l)
  | Minimize(l) -> Printf.sprintf("Minimize\n%s") ( list_intercalate string_of_linear_expr "\n" l)

let string_of_lp lp =
  match lp with
  | LP(o, c, b, t, s) -> Printf.sprintf("%s\n%s\n%s\n%s\n%s\n")
    (string_of_objective o)
    (string_of_constraints c)
    (string_of_bounds b)
    (string_of_types t)
    "End"
