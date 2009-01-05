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

let () =
  let st = T.create () in
  let date = "2004-10-01" in
  let root = "/Users/alexyk/cells" in
  Seq.st_dirwalk add_stree st ~date root;

(* T.tree st *)
