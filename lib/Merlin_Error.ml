exception Not_Implemented

(* Parsing and preprocessing *)
exception Policy_parse_error of string

exception Topology_parse_error

exception Predicate_parse_error

exception Redefined_symbol of string

exception Undefined_symbol of string

exception Missing_quantifier

exception Unknown_function of string

exception Invalid_function_arguments of string

exception Invalid_rate_var of string option

exception Non_local_policy
(* Solver *)

exception No_valid_solution

exception Unexpected_epsilon

(* Code generation *)

exception Unrealizable_path of Merlin_Types.regex

exception Invalid_statement_id of string

exception Invalid_instruction_type of Merlin_Types.instruction

exception Unspecified_port

exception Invalid_hop_sequence of Merlin_Types.hop * Merlin_Types.hop

exception Unknown_click_function of string

exception Invalid_host_type of string

exception Undeclared_host_or_function of string

exception Unknown_location of string

exception Impossible_group_union of Frenetic_OpenFlow.group * Frenetic_OpenFlow.group

exception Unimplementable_tc_predicate of Merlin_Types.pred
