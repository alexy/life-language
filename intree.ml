(* Testing Generalized Suffix Trees
  
   for generalized alphabet sequences 
   -- for integer sequences from toplevel
  
   Copyright (c) 2009, Alexy Khrabrov, Cicero Institute
   Author: Alexy Khrabrov <deliverable@gmail.com>  
   License: LGPL
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
