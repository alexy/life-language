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
  
type clist = int * int list
type hitmax = 
  | Both of (clist * clist)
  | Hit of clist | Max of clist | Miss

let nonempty a = Array.length a > 0

let top_ids a =
  let len = Array.length a in
  if len = 0 then (0,[])
  else
    let c = snd a.(0) in
    let rec go i acc =
      if i >= len || snd a.(i) <> c then acc
      else go (i+1) ((fst a.(i))::acc)
    in
    (c, go 0 [])

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
    Both (top_ids a1, top_ids a2)
  else if nonempty a1 then
    Hit (top_ids a1)
  else if nonempty a2 then 
    Max (top_ids a2)
  else Miss
  
let show_ids ?(max=3) li =
  let l = Utils.take max li in
  let l's = List.map string_of_int l in
  String.concat "|" l's
  
let do_sample t sample =
  let s = Array.of_list sample in begin
  match suffice t s with
  | Both ((_,s1),(_,s2)) -> printf " %s,%s" (show_ids s1) (show_ids s2)
  | Hit (_,s) -> printf " %s!" (show_ids s)
  | Max (_,s) -> printf " %s?" (show_ids s)
  | Miss -> printf " *"
  end
  (* ; print_endline "" *)

let incr_ids a li =
  List.iter (fun i -> a.(i)  <- a.(i) + 1) li
  
let tally_sample t (a1,a2) sample =
  let s = Array.of_list sample in
  match suffice t s with
  | Both ((_,s1),(_,s2)) -> begin incr_ids a1 s1; incr_ids a2 s2 end
  | Hit (_,s) -> incr_ids a1 s
  | Max (_,s) -> incr_ids a2 s
  | Miss -> ()
  
let show_array a = 
  let li = Array.to_list a in
  let lis = List.map (fun (x,y) -> sprintf "(%d,%d)" x y) li in
  "[|"^(String.concat ";" lis)^"|]"
  
let sort_desc =  Array.sort (fun (x,_) (y,_) -> compare y x)
  
let pos_array a =
  let len = Array.length a in
  let b = Array.init len (fun i -> (a.(i),i)) in
  b
  
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
  (* List.iter (do_sample st) samples; *)
  let a1 = Array.make 100 0 in
  let a2 = Array.make 100 0 in
  List.iter (tally_sample st (a1,a2)) samples;
  let b1 = pos_array a1 in
  let b2 = pos_array a2 in
  sort_desc b1;
  sort_desc b2;
  print_endline (show_array b1);
  print_endline (show_array b2)
