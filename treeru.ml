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

let pair_compare (x,y) (x',y') =
  if y <> y' then compare y' y else compare x x'
  
let sort_hash h =
  let li = Hashtbl.fold (fun k v acc -> (k,v)::acc) h [] in
  let a = Array.of_list li in
  Array.sort pair_compare a;
  a

let incr_hash h k =
  let v = if Hashtbl.mem h k then
  Hashtbl.find h k else 0 in
  Hashtbl.replace h k (v+1)
  
type hitmax = 
  | Both of ((int * int) * (int * int))
  | Hit of (int * int) | Max of (int * int) | None

let nonempty a = Array.length a > 0

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
  if nonempty a1 && nonempty a2 then 
    Both (a1.(0),a2.(0))
  else if nonempty a1 then
    Hit a1.(0)
  else if nonempty a2 then 
    Max a2.(0)
  else None
  
let do_sample t sample =
  let s = Array.of_list sample in begin
  match suffice t s with
  | Both ((s1,_),(s2,_)) -> printf " %d,%d" s1 s2
  | Hit (s,_) -> printf " %d!" s
  | Max (s,_) -> printf " %d?" s
  | None -> printf " *"
  end
  (* ; print_endline "" *)
  
let () =
  let st = T.create () in
  let date = "2004-10-01" in
  let root = "/Users/alexyk/cells" in
  Seq.st_dirwalk add_stree st ~date root;

  (* let a = count_leaves st in
  print_endline (show_pairs a); *)

  let sample_file = Sys.argv.(1)
    (* "/Users/alexyk/cells/input/sample-list_2004-10-01_s10_x10-4_p1" *)
  in
  let samples = Seq.read_many sample_file in
  List.iter (do_sample st) samples;
  ()
