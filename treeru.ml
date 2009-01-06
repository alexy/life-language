open Printf
(* NB: the below is needed since 
  intseq.ml contains the declaration
  module Intseq : Suffix.ALPHABET = sig ... end 
  we could open Intseq, but it would be confusing;
  can we somehow avoid module wrapping for signatures
  and use the raw file conforming to a signature,
  or simulate auto-export of the inner Intseq somehow? *)
module Intseq=Intseq.Intseq 

module Visible = struct 
  module A=Intseq 
  let get_visible _ = (0,0) 
end

module T = Suffix.Tree (Intseq) (Visible)

let add_stree st date dir =
  let datafile = Seq.dir_data dir date in
  let cells = Array.of_list (Seq.read_cell_line datafile) in
  ignore (T.add st (Intseq.of_array cells));
  printf "added cells from %s, suffix tree size is now %d\n" datafile (T.size st); 
  flush stdout

let walk_leaves f acc t =
  let ft = f t in 
  let rec go ft acc n =
    if T.is_leaf t n then ft acc n
    else let children_set = T.children t n in
    let children_list = LSet.elements children_set in
    List.fold_left (go ft) acc children_list
  in
  go ft acc (T.root t)

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
  walk_leaves count_nonempty a t
  
let show_pairs a =
  let one p = sprintf "(%d,%d)" (fst p) (snd p) in
  "["^(String.concat ";" (List.map one (Array.to_list a)))^"]"

let () =
  let st = T.create () in
  let date = "2004-10-01" in
  let root = "/Users/alexyk/cells" in
  Seq.st_dirwalk add_stree st ~date root;
  let a = count_leaves st in
  print_endline (show_pairs a)
