let sprintf = Printf.sprintf

(* can this be done with one Pcre.<exec-type-op>?: *)
let elem_match list regex's =
  let re = Pcre.regexp regex's in
  try
    let elem = List.find (fun x -> Pcre.pmatch ~rex:re x) list in
    let meat = (Pcre.extract ~rex:re elem).(1) in
    Some meat
  with Not_found -> None

let rematch elem regex's =
  let re = Pcre.regexp regex's in
  try
    let meat = (Pcre.extract ~rex:re elem).(1) in
    Some meat
  with Not_found -> None


let join ?(sep=" ") ili =
  let sli = List.map string_of_int ili in
  String.concat sep sli

let rec take n l =
  let rec go n l acc =
  match l with
  | x::xs when n > 0 -> go (n-1) xs (x::acc) 
  | _ -> List.rev acc in
  go n l []
  
let rec drop n l =
  let rec go n l =
  match l with
  | x::xs when n > 0 -> go (n-1) xs 
  | xs -> xs in
  go n l

(* these may be not very efficient, perhaps use fold or direct acc? *)

let rec odd  = function | x::_::xs -> x::(odd xs)  | x -> x
let rec even = function | _::x::xs -> x::(even xs) | _::x -> x | _ -> []

let rec odd_even o e = 
  match o,e with
  | x::xs,y::ys -> x::y::(odd_even xs ys)
  | xs,[] -> xs
  | [],ys -> ys
  

(* The Fisher-Yates shuffle (aka Knuth Shuffle), by Martin Bishop,
   http://forge.ocamlcore.org/snippet/detail.php?type=snippet&id=1
 *)

let shuffle a =
  for n = ((Array.length a) - 1) downto 0 do
    Random.self_init ();
    let k = Random.int (n + 1) in
    let temp = a.(n) in
      a.(n) <- a.(k);
      a.(k) <- temp
  done
  
let which_ith list x =
  let rec go l x i =
    match l with
    | x'::xs -> if x = x' then Some i else go xs x (i+1)
    | _ -> None
  in go list x 0
  
let range ?(from=1) upto =
  (* let range ?from upto = let from = match from with | Some x -> x | None -> 1 in *)
  let rec go from upto acc =
    if from > upto then acc else go from (upto-1) (upto::acc)
  in
  go from upto []
  
let range0 = range ~from:0

let show_intlist_meat l =
  let l's = List.map string_of_int l in
  (String.concat ";" l's)
  
let show_intlist l =
  let meat = show_intlist_meat l in
  sprintf "[%s]" meat

let show_intarray a = 
  let ali = Array.to_list a in
  let meat = show_intlist_meat ali in
  sprintf "[|%s|]" meat

let show_floatlist_meat l =
  let l's = List.map string_of_float l in
  (String.concat ";" l's)
  
let show_floatlist l =
  let meat = show_floatlist_meat l in
  sprintf "[%s]" meat

let show_floatarray a = 
  let ali = Array.to_list a in
  let meat = show_floatlist_meat ali in
  sprintf "[|%s|]" meat
  
let sum_intlist =
  List.fold_left (fun e acc -> e+acc) 0
  
let incr_hash h k =
  let v = if Hashtbl.mem h k then
  Hashtbl.find h k else 0 in
  Hashtbl.replace h k (v+1)

let show_hash =
  Hashtbl.iter (fun a b -> Printf.printf "%d => %d\n" a b)

let pair_compare (x,y) (x',y') =
  if y <> y' then compare y' y else compare x x'
  
let sort_hash h =
  let li = Hashtbl.fold (fun k v acc -> (k,v)::acc) h [] in
  let a = Array.of_list li in
  Array.sort pair_compare a;
  a

let show_pairs li =
  let one p = Printf.sprintf "(%d,%d)" (fst p) (snd p) in
  "["^(String.concat ";" (List.map one li))^"]"

let rec graph_pairs = function
  | (a,b)::rest -> begin 
      Printf.printf "%d\t%d\n" a b;
      graph_pairs rest
    end
  | _ -> ()
  
(* Sys.file_exists *)
let file_exists name =
  try begin Unix.stat name; true end
  with Unix.Unix_error _ -> false
  
let directory_exists name =
  try let s = Unix.stat name in s.Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false
  