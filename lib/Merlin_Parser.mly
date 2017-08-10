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
%token<Merlin_Types.info> AMP BAR BANG AND OR NOT
%token<Merlin_Types.info> TILDE BACKSLASH COMMA DOT COLON SEMI IN FWDSLASH
%token<Merlin_Types.info> FORALL FOREACH EXISTS EXPAND
%token<Merlin_Types.info * float> FLOAT
%token<Merlin_Types.info * string> STRING IDENT
%token<Merlin_Types.info * Int64.t> INT64 IPADDR MACADDR HEX

%token<Merlin_Types.info> SWITCH PORT SRCMAC DSTMAC FRAMETYPE VLAN VLANPCP SRCIP DSTIP PROTOCOLTYPE TCPSRCPORT TCPDSTPORT
%token<Merlin_Types.info> ARP IP ICMP TCP UDP

%token<Merlin_Types.info> NONE

%token<Merlin_Types.info> PHYS FN
%token<Merlin_Types.info> TRUE FALSE
%token<Merlin_Types.info> CROSS ZIP DISTINCT

%left BAR
%left STAR
%right NOT

%type <Merlin_Types.program> program
%type <Merlin_LPTypes.lp> lp

%start program
%start lp

%%

/* ------ COMMON FORMS ------*/

and_:
  | AND     { () }
  | AMP AMP { () }

or_:
  | OR      { () }
  | BAR BAR { () }

not_:
  | NOT  { () }
  | BANG { () }


/* ----- CORE LANGUAGE ----- */

policies:
  | core_policy policies
      { $1 :: $2 }
  | core_policy { [ $1 ] }
  |
    {[]}

core_policy:
  | LBRACK core_statements RBRACK core_formula
      { Policy($2, $4) }

core_statements:
   | core_statement SEMI core_statements
       { $1 :: $3 }
   | core_statement
       { [$1] }

core_statement:
   | IDENT COLON LPAREN predicate RPAREN ARROW regex
       { let _,id = $1 in Statement (id, $4, $7)}

core_formula:
   | COMMA core_oformula
       { $2 }
   |
       { FNone }

core_oformula:
   | core_oformula or_ core_aformula
       { FOr($1,$3) }
   | core_aformula
       {  $1 }

core_aformula:
   | core_aformula and_ core_nformula
       { FAnd($1, $3) }
   | core_nformula
       { $1 }

core_nformula:
   | not_ core_xformula
       { FNeg $2 }
   | core_xformula
       { $1 }

core_xformula:
   | MAX LPAREN core_bexpr COMMA rate RPAREN
       { FMax($3, (Int64.of_float $5)) }
   | MIN LPAREN core_bexpr COMMA rate RPAREN
       { FMin($3, (Int64.of_float $5)) }
   | TRUE
       { FNone }

core_bexpr:
   | core_bexpr PLUS core_bexpr_atom
       { BSum($1,$3) }
   | core_bexpr_atom
       { $1 }

core_bexpr_atom:
   | rate
       { BLit (Int64.of_float $1) }
   | IDENT
       { let _,i = $1 in BVar i }

/* ----- COMPLETE LANGUAGE ----- */
program:
 | names blocks policies
     { Program { names = symbol_table
               ; blocks = $2
               ; policies = $3 } }

names :
 | name names
     { $1 }
 |
     { () }

name:
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

/* ------ MACRO LANGUAGE --- --- */

blocks :
   | block blocks
       { $1 :: $2 }
   | block { [ $1 ] }
   |
     { [] }

block:
   | iterator IN expander COLON predicate ARROW regex rate_stmt SEMI
        {
          Block { iter = $1
                ; expander = $3
                ; pred = $5
                ; regex = $7
                ; rate = $8 } }

iterator:
   | FOREACH pair
        { let s,d = $2 in Foreach(s,d) }

pair:
   | LPAREN IDENT COMMA IDENT RPAREN
       { let _,i = $2 in
         let _,j = $4 in
         (i, j) }

expander:
   | ZIP LPAREN container COMMA container RPAREN
       { Zip($3,$5) }
   | CROSS LPAREN container COMMA container RPAREN
       { Cross($3,$5) }
   | DISTINCT LPAREN container COMMA container RPAREN
       { Distinct($3,$5) }

container:
   | IDENT
       { let _,n = $1 in Name n }
   | set_literal { Set $1 }

/* ----- PREDICATES ----- */

predicate :
  | predicate or_ apredicate
      { Or ($1, $3) }
  | apredicate
      { $1 }

apredicate:
  | apredicate and_ upredicate
      { And ($1, $3) }
  | upredicate
      { $1 }

upredicate:
  | not_ upredicate
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
      { let _,i = $1 in Char (get_symbol i) }
  | IPADDR
      { let _,i = $1 in
        let i = Int64.to_string i in
        Char(get_symbol i) }

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
