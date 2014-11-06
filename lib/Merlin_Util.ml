open Unix
open Buffer

let buffer_intercalate buf f s ls =
  List.iter (fun l -> (f l buf); add_string buf s) ls

let intercalate f s = function
  | [] ->
    ""
  | h::t ->
    List.fold_left (fun acc x -> acc ^ s ^ f x) (f h) t

let concat_map f lst =
  List.fold_right (fun a bs -> List.append (f a) bs) lst []

let rec filter_map f xs = match xs with
  | [] -> []
  | x :: xs' -> match f x with
    | Some y -> y :: (filter_map f xs')
    | None -> filter_map f xs'

let filter_none lst = filter_map (fun x -> x) lst

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let load_lines f =
  let str = load_file f in
  Str.split (Str.regexp "\n") str

let write_to_file filename text_to_write =
  try
    let cout = open_out filename in
    let co = Format.formatter_of_out_channel cout in
    Format.fprintf co "%s\n" text_to_write;
    close_out cout
  with Sys_error _ as e ->
    Format.printf "Cannot open file \"%s\": %s\n" filename (Printexc.to_string e)

let walk_directory_tree dir pattern =
  let select str = Str.string_match (Str.regexp pattern) str 0 in
  let rec walk acc = function
  | [] -> (acc)
  | dir::tail ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs,files) f ->
             match (stat f).st_kind with
             | S_REG -> (dirs, f::files)  (* Regular file *)
             | S_DIR -> (f::dirs, files)  (* Directory *)
             | _ -> (dirs, files)
          ) ([],[]) contents
      in
      let matched = List.filter (select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]

(*  Network utilities *)
let ip_of_string s =
  let parse_decbyte str = Int32.of_string str in
  let open Int32 in
      let bytes = Str.split (Str.regexp "\\.") s in
      (logor (shift_left (parse_decbyte (List.nth bytes 0)) 24)
         (logor (shift_left (parse_decbyte (List.nth bytes 1)) 16)
            (logor (shift_left (parse_decbyte (List.nth bytes 2)) 8)
               (parse_decbyte (List.nth bytes 3)))))

let connect_to ip port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let inaddr = Unix.inet_addr_of_string ip in
  let socketaddr = Unix.ADDR_INET(inaddr, port) in
  let _ = Unix.connect socket socketaddr in
  socket

let local_bind_to port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  setsockopt socket SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (inet_addr_of_string "0.0.0.0" , port));
  socket

let recv_all socket =
    let buffer = String.create 512 in
    let rec _readall accum =
        try
            let count = (recv socket buffer 0 512 []) in
                if count < 512 then (String.sub buffer 0 count)::accum
                else _readall ((String.sub buffer 0 count)::accum)
        with _ ->
            accum
    in
        String.concat "" (List.rev (_readall []))

let send_all socket str =
  let rec _sendall socket str offset amt =
    let sent = Unix.send socket str offset amt [] in
    if sent < amt then
      _sendall socket str (offset+sent) (amt-sent) in
  _sendall socket str 0 (String.length str)

let rec recv_till socket accum delim =
  let buffer = String.create 512 in
  try
    let count = (recv socket buffer 0 512 []) in
    if count = 0 then
      ""
    else
      let tmp = accum ^ (String.sub buffer 0 count) in
      if String.contains tmp delim then
        tmp
      else
        recv_till socket tmp delim
  with _ -> Printf.printf "Empty message!\n%!"; buffer

let send_to ip string =
  let socket = connect_to ip 1773 in
  send_all socket string

(* JNF-ENHANCED SETS AND MAPS *)

let list_intercalate f sep l =
  List.fold_left
    (fun acc li -> Printf.sprintf "%s%s%s" acc (if acc = "" then "" else sep) (f li)) "" l


module type OrderedType = Set.OrderedType

module Mapplus =
struct
  module type S = sig
    include Map.S
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end
  module Make (Ord:OrderedType) =
  struct
    include Map.Make(Ord)
    let keys m = let ks,_ = List.split (bindings m) in ks
    let values m = let _,vs = List.split (bindings m) in vs
  end
end

module Setplus =
struct
  module type S = sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val intercalate : (elt -> string) -> string -> t -> string
    val of_list : elt list -> t
  end
  module Make (Ord:OrderedType) =
  struct
    include Set.Make(Ord)
    let map f s = fold (fun v acc -> add (f v) acc) s empty
    let intercalate f sep s =
      fold
        (fun si acc -> Printf.sprintf "%s%s%s" acc (if acc = "" then "" else sep) (f si))
        s ""

    let of_list ls =
      List.fold_left (fun acc e -> add e acc) empty ls
  end
end
