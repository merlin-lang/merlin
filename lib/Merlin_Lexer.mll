{
  open Merlin_Parser

  exception Lexing_error of string

  let lexeme = Lexing.lexeme

  let sprintf = Printf.sprintf

  let filename =
    ref ""

  let set_filename fn =
    filename := fn

  let linenum =
    ref 1

  let set_linenum n =
    linenum := n

  let linestart =
    ref 0

  let set_linestart n =
    linestart := n

  let newline lexbuf =
    incr linenum;
    linestart := Lexing.lexeme_start lexbuf

  let info lexbuf =
    let c1 = Lexing.lexeme_start lexbuf in
    let c2 = Lexing.lexeme_end lexbuf in
    let l = !linenum in
    ((l, c1 - !linestart - 1),(l, c2 - !linestart - 1))

  let error lexbuf msg =
    let i = info lexbuf in
    let t = lexeme lexbuf in
    let s =
      Printf.sprintf "%s: lexing error %s at %s\n"
        (Merlin_Pretty.string_of_info !filename i)
        msg
        t in
    raise (Lexing_error s)

  let keywords = Hashtbl.create 25
  let _ =
    List.iter (fun (kw,tok) -> Hashtbl.add keywords kw tok)
      [
        ("at", fun i -> AT i)
        ; ("max", fun i -> MAX i)
        ; ("min", fun i -> MIN i)
        ; ("foreach", fun i -> FOREACH i)
        ; ("forall", fun i -> FORALL i)
        ; ("exists", fun i -> EXISTS i)
        ; ("expand", fun i -> EXPAND i)
        ; ("Maximize", fun i -> MAXIMIZE i)
        ; ("Minimize", fun i -> MINIMIZE i)
        ; ("Subject", fun i -> SUBJECT i)
        ; ("To", fun i -> TO i)
        ; ("Bounds", fun i -> BOUNDS i)
        ; ("Binary", fun i -> BINARY i)
        ; ("Integers", fun i -> INTEGERS i)
        ; ("Generals", fun i -> GENERALS i)
        ; ("Semis", fun i -> SEMIS i)
        ; ("SOS", fun i -> SOS i)
        ; ("End", fun i -> END i)

        ; ( "switch" , fun i -> SWITCH i)
        ; ( "port" , fun i -> PORT i)
        ; ( "ethSrc" , fun i -> SRCMAC i)
        ; ( "ethDst" , fun i -> DSTMAC i)
        ; ( "ethTyp" , fun i -> FRAMETYPE i)
        ; ( "vlanId" , fun i -> VLAN i)
        ; ( "vlanPcp" , fun i -> VLANPCP i)
        ; ( "ipSrc" , fun i -> SRCIP i)
        ; ( "ipDst" , fun i -> DSTIP i)
        ; ( "ipProto" , fun i -> PROTOCOLTYPE i)
        ; ( "tcpSrcPort" , fun i -> TCPSRCPORT i)
        ; ( "tcpDstPort" , fun i -> TCPDSTPORT i)
        ; ( "arp" , fun i -> ARP i)
        ; ( "ip" , fun i -> IP i)
        ; ( "icmp" , fun i -> ICMP i)
        ; ( "tcp" , fun i -> TCP i)
        ; ( "udp" , fun i -> UDP i)

        ; ("Phys", fun i -> PHYS i)
        ; ("Fn", fun i -> FN i)
        ; ("and", fun i -> AND i)
        ; ("or", fun i -> OR i)
        ; ("not", fun i -> NOT i)
        ; ("in", fun i -> IN i)
        ; ("true", fun i -> TRUE i)
      ]

  let int_of_hex = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
    | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'A' | 'a' -> 10 | 'B' | 'b' -> 11 | 'C' | 'c' -> 12
    | 'D' | 'd' -> 13 | 'E' | 'e' -> 14 | 'F' | 'f' -> 15
    | n -> failwith ("Lexer.int_of_hex: " ^ (String.make 1 n))

  let parse_byte str = Int64.of_string ("0x" ^ str)
  let parse_decbyte str = Int64.of_string str

}

let whitespace = [' ' '\t']+
let newline = "\n"
let uid_char = ['A'-'Z']

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let decimal = ['0'-'9']+
let float_ = ['0'-'9']+ '.' ['0'-'9']+
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let int_char = ['0' - '9']
let hex_char = ['0' - '9' 'A' - 'F' 'a' - 'f']
let string = '"' [^'"']* '"'
let byte = ['0'-'9' 'a'-'f' 'A'-'F']?  ['0'-'9' 'a'-'f' 'A'-'F']
let decbyte =
  (['0'-'9'] ['0'-'9'] ['0'-'9']) | (['0'-'9'] ['0'-'9']) | ['0'-'9']

rule main = parse
      | whitespace         { main lexbuf }
      | "*)"               { error lexbuf "this is not the end of a comment" }
      | "->"               { ARROW (info lexbuf) }
      | ":="               { ASSIGN (info lexbuf) }
      | "("                { LPAREN (info lexbuf) }
      | ")"                { RPAREN (info lexbuf) }
      | "["                { LBRACK (info lexbuf) }
      | "]"                { RBRACK (info lexbuf) }
      | "&"                { AMP (info lexbuf) }
      | "-"                { MINUS (info lexbuf) }
      | "+"                { PLUS (info lexbuf) }
      | "*"                { STAR (info lexbuf) }
      | "~"                { TILDE (info lexbuf) }
      | "="                { EQUALS (info lexbuf) }
      | "<="               { LEQ (info lexbuf) }
      | ">="               { GEQ (info lexbuf) }
      | "|"                { BAR (info lexbuf) }
      | "{"                { LBRACE (info lexbuf) }
      | "}"                { RBRACE (info lexbuf) }
      | "<"                { LANGLE (info lexbuf) }
      | ">"                { RANGLE (info lexbuf) }
      | ","                { COMMA (info lexbuf) }
      | "."                { DOT (info lexbuf) }
      | ":"                { COLON (info lexbuf) }
      | ";"                { SEMI (info lexbuf) }
      | "\\"               { BACKSLASH (info lexbuf) }
      | "/"                { FWDSLASH (info lexbuf) }
      | "!"                { BANG (info lexbuf) }
      | "\""               {
        let s = string "" lexbuf in
        STRING(info lexbuf,s)
      }
      | id as ident {
        try Hashtbl.find keywords ident (info lexbuf)
        with Not_found -> IDENT(info lexbuf, ident)
      }
      | "0x" (hex_char* as hs) {
        let n = "0x" ^ hs in
        let h = Int64.of_string n in
        HEX(info lexbuf,h)
      }
      | float_ as f
          { FLOAT (info lexbuf, float_of_string f)
          }

      | decimal as integ {
        INT64(info lexbuf,Int64.of_string integ)
      }
      | (byte as n6) ":" (byte as n5) ":" (byte as n4) ":" (byte as n3) ":"
          (byte as n2) ":" (byte as n1)
          { let open Int64 in
            MACADDR(info lexbuf,
                    (logor (shift_left (parse_byte n6) 40)
                       (logor (shift_left (parse_byte n5) 32)
                          (logor (shift_left (parse_byte n4) 24)
                             (logor (shift_left (parse_byte n3) 16)
                                (logor (shift_left (parse_byte n2) 8)
                                   (parse_byte n1))))))) }
      | (decbyte as b4) "." (decbyte as b3) "." (decbyte as b2) "." (decbyte as b1)
          { let open Int64 in
            IPADDR(info lexbuf,
              (logor (shift_left (parse_decbyte b4) 24)
                 (logor (shift_left (parse_decbyte b3) 16)
                    (logor (shift_left (parse_decbyte b2) 8)
                       (parse_decbyte b1))))) }
          
      | newline            { newline lexbuf; main lexbuf }
      | eof                { EOF (info lexbuf) }
      | "(*"               { comment lexbuf; main lexbuf }
      | _                  { error lexbuf "unknown token" }

and escape el = parse
    | "\\"          { "\\" }
    | "b"           { "\008" }
    | "n"           { "\010" }
    | "r"           { "\013" }
    | "t"           { "\009" }
    | "0x" (hex_char as h1) (hex_char as h2) {
      String.make 1 (Char.chr (16 * int_of_hex h1 + int_of_hex h2))
    }

    | int_char int_char int_char as c {
      String.make 1 (Char.chr (int_of_string c))
    }
    | _ {
      try List.assoc (lexeme lexbuf) el
      with Not_found -> error lexbuf "in escape sequence"
    }

and string acc = parse
    | "\\"          { let s = escape [("\"","\"");("'","'")] lexbuf in
                      string (acc ^ s) lexbuf }
    | "\""          { acc }
    | newline ([' ' '\t']* "|")?
        { newline lexbuf; string (acc ^ "\n") lexbuf}
    | eof           { error lexbuf "unmatched '\"'" }
    | _             { string (acc ^ lexeme lexbuf) lexbuf }

and comment = parse
    | "(*"             { comment lexbuf; comment lexbuf }
    | "*)"             { () }
    | newline          { newline lexbuf; comment lexbuf }
    | eof              { error lexbuf "unmatched '(*'" }
    | _                { comment lexbuf }
