

(* Gurobi LP file format *)

type sos = Sos of string | NoSos

type rel_op =
| Eq
| Leq
| Geq

type type_decl = 
| Binary of string list
| Integers of string list
| Semis of string list
| Generals of string list

type types =
  type_decl list

type linear_expr =
| Var of string
| Float of float
| Int of int64
| Plus of linear_expr * linear_expr
| Minus of linear_expr * linear_expr
| Mult of linear_expr * linear_expr
| Div of linear_expr * linear_expr

type const = Constraint of linear_expr * rel_op * linear_expr

type constraints = const list

type bound = Bound of linear_expr * rel_op * linear_expr

type bounds = bound list

type objective =
| Maximize of linear_expr list
| Minimize of linear_expr list

type lp = LP of objective * constraints * bounds * types * sos
