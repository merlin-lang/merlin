%{
  open Frenetic_Packet
  open Merlin_Types
  open Merlin_NFA
  open Merlin_Dictionaries
  open Merlin_LPTypes
  open Frenetic_OpenFlow
  open Merlin_Error

  let hex_of_int len n =
    let rec loop i n acc =
      if i <= 0 then acc
      else
      let n' = n lsr 4 in
      loop (pred i) n' ((n mod 16)::acc) in
    if 0 > n || n > 16 lsl (pred len * 4) then
      failwith (Printf.sprintf "Error: n=%d len=%d" n len)
    else
      loop len n []


  (* Ethernet frame types *)
 let arp : int64  = Int64.of_int 0x806
 let ip  : int64  = Int64.of_int 0x800

  (* Ip protocol types *)
 let icmp : int64 = Int64.of_int 0x01
 let tcp  : int64 = Int64.of_int 0x06
 let udp  : int64 = Int64.of_int 0x11



(*

p ::= -- predicates
   | true
   | false
   | match(hdr, n)
   | p1 and p2
   | p1 or p2
   | not p

r ::= - path expressions
   | empty
   | f
   | r1 . r2
   | r1 | r2
   | r*

e ::= bandwitdh terms
  | n
  | x
  | e1 + e2

phi ::= - presburger formulas
    | max(e, n)
    | min(e, n)
    | phi1 and phi2
    | phi1 or phi2
    | not phi

s ::= -- statements
   | < p, r, x >

pol ::= -- policies
   | [s1,...s2], phi

foreach adds a conjunct to the global formula
for each statement in the expansion

forall adds a summand to a single atom in the global fomula
for each statement in the expansion

*)

%}

%token<Merlin_Types.info> EOF NEWLINE
%token<Merlin_Types.info> AT ARROW MAX MIN ASSIGN

%token<Merlin_Types.info> MAXIMIZE MINIMIZE SUBJECT TO
%token<Merlin_Types.info> BOUNDS BINARY INTEGERS SEMIS GENERALS SOS END

%token<Merlin_Types.info> LANGLE RANGLE LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE

%token<Merlin_Types.info> EQUALS LEQ GEQ PLUS MINUS STAR
%token<Merlin_Types.info> AMP BAR NOT BANG AND OR
%token<Merlin_Types.info> TILDE BACKSLASH COMMA DOT COLON SEMI IN FWDSLASH
%token<Merlin_Types.info> FORALL FOREACH EXISTS EXPAND
%token<Merlin_Types.info * float> FLOAT
%token<Merlin_Types.info * string> STRING IDENT
%token<Merlin_Types.info * Int64.t> INT64 IPADDR MACADDR HEX

%token<Merlin_Types.info> SWITCH PORT SRCMAC DSTMAC FRAMETYPE VLAN VLANPCP SRCIP DSTIP PROTOCOLTYPE TCPSRCPORT TCPDSTPORT
%token<Merlin_Types.info> ARP IP ICMP TCP UDP

%token<Merlin_Types.info> NONE

%token<Merlin_Types.info> PHYS FN
%token<Merlin_Types.info> TRUE

%left BAR
%left STAR
%right NOT

%type <Merlin_Types.ast_program> program
%type <Merlin_Types.pred> predicate
%type <Merlin_Types.policy> ir_policy
%type <Merlin_LPTypes.lp> lp
%type <Merlin_Types.regex> regex
%type <string> pol

%start program
%start lp
%start predicate
%start pol
%start ir_policy
%start regex

%%


/* ----- IR REPRESENTATION ----- */

ir_policy:
  | ir_statements ir_formula
      { Policy($1, $2) }

ir_statements:
   | ir_statement ir_statements
       { $1 :: $2 }
   | ir_statement
       { [$1] }

ir_statement:
   | LPAREN predicate COMMA regex COMMA IDENT RPAREN
       { let _,id = $6 in
         Statement($2,$4,id) }

ir_formula:
   | ir_formula OR ir_aformula
       { FOr($1,$3) }
   | ir_aformula
       {  $1 }

ir_aformula:
   | ir_aformula AND ir_nformula
       { FAnd($1, $3) }
   | ir_nformula
       { $1 }

ir_nformula:
   | NOT ir_xformula
       { FNeg $2 }
   | ir_xformula
       { $1 }

ir_xformula:
   | MAX LPAREN ir_bexpr COMMA rate RPAREN
       { FMax($3, (Int64.of_float $5)) }
   | MIN LPAREN ir_bexpr COMMA rate RPAREN
       { FMin($3, (Int64.of_float $5)) }
   | TRUE
       { FNone }

ir_bexpr:
   | ir_bexpr PLUS ir_bexpr_atom
       { BSum($1,$3) }
   | ir_bexpr_atom
       { $1 }

ir_bexpr_atom:
   | rate
       { BLit (Int64.of_float $1) }
   | IDENT
       { let _,i = $1 in BVar i }

/* ----- SURFACE LANGUAGE ----- */
program:
 | assignments policies
     { ASTProgram($2) }

assignments :
 | assignment assignments
     { $1 }
 |
     { () }

assignment:
 | IDENT ASSIGN set_literal SEMI
     {
       let _,i = $1 in
       if StringHash.mem symbol_table i then
         raise (Redefined_symbol(i))
       else
         StringHash.add symbol_table i $3
     }

set_literal:
 | LBRACE id_ps RBRACE {$2}

policies :
   | policy policies
       { $1 :: $2 }
   | policy { [$1] }

policy:
   | comprehension predicate ARROW regex rate_stmt SEMI
       { ASTPolicy($1, $2, $4, $5)}

comprehension:
   | FOREACH pair COLON expansion
       {Foreach($2, $4)}

pair:
   | LPAREN IDENT COMMA IDENT RPAREN
       { let _,i = $2 in
         let _,j = $4 in
         Pair(i, j) }

expansion:
   | IDENT LPAREN term COMMA term RPAREN
       {
         let _,i = $1 in
         Expansion(i,$3,$5)}

term:
   | IDENT
       {
         let _,i = $1 in
         if not (StringHash.mem symbol_table i) then
           raise (Undefined_symbol(i))
         else
           StringHash.find symbol_table i
       }
   | set_literal { $1 }


/* ----- PREDICATES ----- */

predicate :
  | predicate OR apredicate
      { Or ($1, $3) }
  | apredicate
      { $1 }

apredicate:
  | apredicate AND upredicate
      { And ($1, $3) }
  | upredicate
      { $1 }

upredicate:
  | NOT upredicate
      { Not $2 }
  | xpredicate
      { $1 }

xpredicate:
  | LPAREN predicate RPAREN
      { $2 }
  | STAR
      { Everything }
  | NONE
      { Nothing }
  | field_match
      { $1 }

field_match:
  | SRCMAC EQUALS MACADDR
      { let _,m = $3 in Test (Frenetic_NetKAT.EthSrc m) }
  | DSTMAC EQUALS MACADDR
      { let _,m = $3 in Test (Frenetic_NetKAT.EthDst m) }
  | FRAMETYPE EQUALS frametype
      { Test (Frenetic_NetKAT.EthType $3) }
  | VLAN EQUALS INT64
      { let _,i = $3 in Test (Frenetic_NetKAT.Vlan (Int64.to_int i)) }
  | VLANPCP EQUALS INT64
      { let _,i = $3 in Test (Frenetic_NetKAT.VlanPcp (Int64.to_int i)) }
  | SRCIP EQUALS IPADDR
      { let _,i = $3 in And (Test (Frenetic_NetKAT.EthType 0x0800),
                             Test (Frenetic_NetKAT.IP4Src (Int64.to_int32 i, Int32.of_int 32))) }
  | DSTIP EQUALS IPADDR
      { let _,i = $3 in And (Test (Frenetic_NetKAT.EthType 0x0800),
                             Test (Frenetic_NetKAT.IP4Dst (Int64.to_int32 i, Int32.of_int 32))) }
  | SRCIP EQUALS INT64
      { let _,i = $3 in And (Test (Frenetic_NetKAT.EthType 0x0800),
                             Test (Frenetic_NetKAT.IP4Src (Int64.to_int32 i, Int32.of_int 32))) }
  | DSTIP EQUALS INT64
      { let _,i = $3 in And (Test (Frenetic_NetKAT.EthType 0x0800),
                             Test (Frenetic_NetKAT.IP4Dst (Int64.to_int32 i, Int32.of_int 32))) }
  | SRCIP IN set
      { let _,i = $3 in Test (Frenetic_NetKAT.IP4Src (Int64.to_int32 i, Int32.of_int 32)) }
  | DSTIP IN set
      { let _,i = $3 in Test (Frenetic_NetKAT.IP4Dst (Int64.to_int32 i, Int32.of_int 32)) }
  | PROTOCOLTYPE EQUALS protocoltype
      { Test (Frenetic_NetKAT.IPProto $3) }
  | TCPSRCPORT EQUALS INT64
      { let _,i = $3 in And (Test (Frenetic_NetKAT.IPProto 6),
                             Test (Frenetic_NetKAT.TCPSrcPort (Int64.to_int i))) }
  | TCPDSTPORT EQUALS INT64
      { let _,i = $3 in And (Test (Frenetic_NetKAT.IPProto 6),
                             Test (Frenetic_NetKAT.TCPDstPort (Int64.to_int i))) }

frametype :
  | IP
      { 0x0800 }
  | ARP
      { 0x0806 }
  | INT64
      { let _,i = $1 in Int64.to_int i}

protocoltype:
  | ICMP
      { 0x001 }
  | TCP
      { 0x06 }
  | UDP
      { 0x11 }
  | INT64
      { let _,i = $1 in (Int64.to_int i) }

set:
  | IPADDR FWDSLASH INT64 { $1 }
  | LBRACE ipaddr_pc RBRACE { List.hd $2 }


/* ----- REGULAR EXPRESSIONS ----- */

regex:
  | regex_alt { $1 }

regex_alt:
  | regex_alt BAR regex_cat
      { Alt($1, $3) }
  | regex_cat
      { $1 }

regex_cat:
  | regex_cat regex_kleene
      { Cat($1, $2) }
  | regex_kleene
      { $1 }

regex_kleene :
  | regex_kleene STAR  { Kleene($1) }
  |	regex_neg  { $1 }

regex_neg :
  | BANG regex_neg { $2 }
  |	regex_char  { $1 }

regex_char:
  | IDENT
      { let _,i = $1 in
    if StringHash.mem symbol_table i then
      begin
      (* i is an abstract function *)
      LocationSet.fold
        (fun l acc ->
          match acc with
          | Empty -> Char(get_symbol (l, Some i))
          | _ ->
            Alt(acc, Char(get_symbol (l, Some i))))
        (StringHash.find symbol_table i)
        Empty
      end
    else
      (* i is an abstract location *)
      let s = get_symbol (i,None) in
      Printf.printf "Adding %d -> %s\n" s i;
      Char(s )
      }

  | IPADDR
      { let _,i = $1 in
    let i = Int64.to_string i in
    Char(get_symbol (i, None)) }

  | DOT
      { AnyChar }

  | LPAREN regex RPAREN
      { Group($2) }

rate_stmt:
   | AT MIN LPAREN expr RPAREN MAX LPAREN expr RPAREN
       { let n = $4 in
         let m = $8 in
         RBoth(n, m) }
   | AT MAX LPAREN expr RPAREN
       { let n = $4 in RMax(n) }
   | AT MIN LPAREN expr RPAREN
       { let n = $4 in RMin(n) }
   | /* Unspecified */
       { RNone }

expr:
   | add_expr { Int64.of_float $1 }

add_expr:
   | add_expr PLUS mul_expr { $1 +. $3 }
   | add_expr MINUS mul_expr { $1 -. $3 }
   | mul_expr { $1 }

mul_expr:
   | mul_expr STAR prim_expr { $1 *. $3 }
   | mul_expr FWDSLASH prim_expr { $1 /. $3 }
   | prim_expr { $1 }

prim_expr:
   | rate { $1 }
   | FLOAT { let _,n = $1 in n}

rate:
   | INT64 IDENT
      { let _,n = $1 in
        let _,b = $2 in
        let m =
          match b with
            | "bps" -> 1L
            | "Bps" -> 8L
            | "kbps" -> 1024L
            | "kBps" -> 8192L
            | "Mbps" -> 1048576L
            | "MBps" -> 8388608L
            | "Gbps" -> 1073741824L
            | "GBps" -> 8589934592L
          | _ -> raise Parse_error in
        Int64.to_float (Int64.mul n  m)
      }

/* ----- IR ----- */

bw:
    | expr       { "expr" }

phi:
    | MAX LPAREN bw COMMA expr RPAREN
       { "max" }
    | MIN LPAREN bw COMMA expr RPAREN
       { "max" }

stmt: 
    | LANGLE predicate COMMA regex COMMA term RANGLE { "stmt" }

stmt_pc:
   | stmt COMMA stmt_pc  { $1 :: $3 }
   | stmt  { [$1] }

pol: 
   | LBRACK stmt_pc RBRACK COMMA phi { "pol" }


/* ----- SOLVER LANGUAGE ----- */
lp:
   | objective constraints bounds types sos END
       { LP($1, $2, $3, $4, $5) }

objective:
   | MAXIMIZE linear_expr
       { Maximize([$2]) }
   | MINIMIZE linear_expr
       { Minimize([$2]) }

constraints:
   | SUBJECT TO const_pn
       { $3 }

const:
   | linear_expr rel_op num { Constraint($1,$2,$3) }
//   | label linear_expr quad_expr rel_op INT64
//   | label linear_expr rel_op INT64
//   | linear_expr quad_expr rel_op INT64
//   | linear_expr rel_op INT64

num:
   | INT64 { let _, i = $1 in Int(i) }

rel_op:
   | EQUALS { Eq }
   | LEQ { Leq }
   | GEQ { Geq }

linear_expr:
   | expr_add { $1 }

expr_add:
   | expr_add PLUS expr_mul { Plus($1,$3) }
   | expr_add MINUS expr_mul { Minus($1,$3) }
   | expr_mul { $1 }

expr_mul:
   | expr_mul expr_prim { Mult($1,$2) }
   | expr_prim { $1 }

expr_prim:
   | IDENT { let _, id = $1 in Var(id) }
   | INT64 { let _, i = $1 in Int(i) }
   | FLOAT { let _, f = $1 in Float(f) }

bounds:
   | BOUNDS bound_pn
       { $2 }

bound:
   | linear_expr rel_op num { Bound($1, $2, $3) }

types:
   | type_decl_pn
       { $1 }

type_decl:
   | BINARY id_pw
       { Binary($2) }
   | INTEGERS id_pw
       { Integers($2) }
   | GENERALS id_pw
       { Integers($2) }
   | SEMIS id_pw
       { Integers($2) }

sos:
   | SOS
       { Sos("sos\n") }
   | /* Optional */
       { NoSos }

// ----- lists (_pw = plus whitespace = +) (_pn = plus newline = +\n) (_pc = plus comma = +,)  (_ps = plus semi = +;)

id_pw:
   | IDENT id_pw
     { let _, id = $1 in id :: $2 }
   | { [] }

id_ps:
 | IDENT SEMI id_ps
     {  let _,i = $1 in LocationSet.add i $3 }
 | IDENT
     {  let _,i = $1 in LocationSet.singleton i }

type_decl_pn:
   | type_decl type_decl_pn { $1 :: $2 }
   | { [] }

bound_pn:
   | bound bound_pn { $1 :: $2 }
   | { [] }

const_pn:
   | const const_pn { $1 :: $2 }
   | { [] }

ipaddr_pc:
   | IPADDR COMMA ipaddr_pc  { $1 :: $3 }
   | IPADDR  { [$1] }
