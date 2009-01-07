(* Testing Generalized Suffix Trees
  
   for generalized alphabet sequences 
   -- for integer sequences from toplevel
  
   Copyright (c) 2009, Alexy Khrabrov, Cicero Institute
   Author: Alexy Khrabrov <deliverable@gmail.com>  
   License: LGPL
   
   This file contains functions and snippets tested in toplevel,
   it may not load in its entirety right away
 *)


#directory "/s/src/ocaml/suffix/ferre"
#load "cis.cmo"
#load "lSet.cmo"
#load "suffix.cmo"
#use  "intseq.ml"

#require "str"
#load "seq.cmo"
open Printf

module Visible = struct 
  module A=Intseq 
  let get_visible _ = (0,0) 
end

module T = Suffix.Tree (Intseq) (Visible)

let add_stree st date dir =
  let datafile = Seq.dir_data dir date in
  let cells = Array.of_list (Seq.read_cell_line datafile) in
  T.add st (Intseq.of_array cells);
  printf "added cells from %s, suffix tree size is now %d\n" datafile (T.size st); 
  flush stdout


(* 
let st = T.create () in
let date = "2004-10-01" in
let root = "/Users/alexyk/cells" in
Seq.st_dirwalk add_stree st ~date root;
T.tree st
 *)


 (* 
1 2 3 1 2 3 4 1 2 3
1 2 1 2 3 7 8 1 2 5   
  *)
let st = T.create ()
let date = "2004-10-01"
let root = "/Users/alexyk/cells/test"
Seq.st_dirwalk add_stree st root;
T.tree st

let walk_strings id seq acc = (Intseq.to_string seq)::acc

let walk_leaves f acc t =
  let ft = f t in 
  let rec go ft acc n =
    if T.is_leaf t n then ft acc n
    else let children_set = T.children t n in
    let children_list = LSet.elements children_set in
    List.fold_left (go ft) acc children_list
  in
  go ft acc (T.root t)
  
let collect f tree acc node =
  let one = f tree node in
  let one's = Intseq.to_string one in
  one's::acc

let collect_labels = collect T.label
let collect_paths = collect T.path
  
let show_leaves = walk_leaves collect_labels []
let show_paths  = walk_leaves collect_paths []


let suffixes tree acc node =
  let strid,pos = T.suffix tree node in
  let res = sprintf "(%d,%d)" strid pos in
  res::acc
  
let show_suffixes = walk_leaves suffixes []

let walk_nodes f acc t =
  let ft = f t in 
  let rec go ft acc n =
    if T.is_leaf t n then acc
    else begin
      
      (* -- seeems there's no nodes with ext size == 1?
         let ext = T.ext t n in
         let ext_size = LSet.cardinal ext in
         let acc = if ext_size = 1 then *)
      
      let acc = if T.is_maximal t n then
        ft acc n
      else acc in 
      let children_set = T.children t n in
      let children_list = LSet.elements children_set in
      List.fold_left (go ft) acc children_list
    end
  in
  go ft acc (T.root t)

let show_maxims = walk_nodes collect_paths []  

let s1 = T.create ()
T.add s1 (Intseq.of_array [|1;2;3|])
T.add s1 (Intseq.of_array [|1;2;4|])

let s2 = T.create ()
T.add s2 (Intseq.of_array [|1;2;3|])
T.add s2 (Intseq.of_array [|1;2;1;2|])

let s3 = T.create ()
T.add s3 (Intseq.of_array [|1;2;3|])


let collect_nonempty f tree acc node =
  let label = T.label tree node in
  let label_empty = Intseq.is_empty label in
  if  label_empty then 
    acc 
  else
    let label_length = Intseq.length label in
    let suffix = T.suffix tree node in
    (* don't really need empty string check *)
    let one = f tree node in
    let one's = Intseq.to_string one in
    if String.length one's > 0 then
      (one's, label_length, suffix)::acc
    else
      acc

let collect_nonempty_labels = collect_nonempty T.label
let collect_nonempty_paths  = collect_nonempty T.path

let show_nonempty_leaves = walk_leaves collect_nonempty_labels []
let show_nonempty_paths  = walk_leaves collect_nonempty_paths  []


let factor_path's t a =
  let a,s,b = T.find_factor t (Intseq.of_array a) in
  let leaf's, info = if T.is_leaf t b then
    let strid,pos = T.suffix t b in
    let strid's = string_of_int strid in  
    "yes leaf", strid's 
  else
    let ext = T.ext t b in
    let exts = List.map string_of_int (LSet.elements ext) in
    let ext's = "[" ^ (String.concat "," exts) ^ "]" in
    "not leaf", ext's in
  let [a';s'] = List.map Intseq.to_string [(T.path t a); s] in
  printf "%s, %s, -- leaf: %s, info: %s\n" a' s' leaf's info
  
  
type leafnode = Leaf of int | Node of int array
let factor_path t a =
  let a,s,b = T.find_factor t (Intseq.of_array a) in
  if T.is_leaf t b then
    let strid,pos = T.suffix t b in
    Leaf strid 
  else
    let ext = T.ext t b in
    let extarray = Array.of_list (LSet.elements ext) in
    Node extarray


let subseqs s =
  let length = Array.length s in
  let last = length - 1 in
  for start = 0 to last do
    for len = 1 to (length-start) do
      let sub = Array.sub s start len in
      let sub's = String.concat ";" (List.map string_of_int (Array.to_list sub)) in
      printf "%d..%d: %s\n" start len sub's
    done
  done
  
(* NB: count #uniq subseqs for each strid in a suffix tree *)

let count_nonempty tree acc node =
  let strid,_ = T.suffix tree node in
  let i = strid - 1 in
  let x,y = acc.(i) in
  let label = T.label tree node in
  let label_empty = Intseq.is_empty label in
  if  label_empty then
    acc.(i) <- (x+1,y)
  else
    acc.(i) <- (x,y+1);
  acc
    
let count_leaves t =
  let a = Array.make (T.size t) (0,0) in
  walk_leaves count_nonempty a t;
  
let show_pairs a =
  let one p = sprintf "(%d,%d)" (fst p) (snd p) in
  "["^(String.concat ";" (List.map one (Array.to_list a)))^"]"
  
let incr_hash h k =
  let v = if Hashtbl.mem h k then
  Hashtbl.find h k else 0 in
  Hashtbl.replace h k (v+1)

let show_hash =
  Hashtbl.iter (fun a b -> printf "%d => %d\n" a b)

  
let show_hashes t s =
  let h1,h2 = suffice t s in
  print_endline "leaves:";
  show_hash h1;
  print_endline "nodes:";
  show_hash h2

(* NB modify T.add to record the number of passes by each strid through extent *)

let pair_compare (x,y) (x',y') =
  if y <> y' then compare y' y else compare x x'
  
let sort_hash h =
  let li = Hashtbl.fold (fun k v acc -> (k,v)::acc) h [] in
  let a = Array.of_list li in
  Array.sort pair_compare a;
  a
  
let suffice t s =
  let h1 = Hashtbl.create 1000 in
  let h2 = Hashtbl.create 1000 in
  let length = Array.length s in
  let last = length - 1 in
  for start = 0 to last do
    for len = 1 to (length-start) do
      let sub = Array.sub s start len in
      try
        let fact = factor_path t sub in
        match fact with
        | Leaf i -> incr_hash h1 i
        | Node a -> Array.iter (incr_hash h2) a
      with Not_found -> ()
    done
  done;
  (* (h1, h2) *)
  (* NB: break ties in a1 via a2? *)
  let a1 = sort_hash h1 in
  let a2 = sort_hash h2 in
  if Array.length a1 > 0 then Some a1.(0)
  else if Array.length a2 > 0 then Some a2.(0)
  else None