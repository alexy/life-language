open Printf

let do_dir ?mincount ?date dir =
  printf "doing dir: %s\n" dir
  
let () =
  let date = 
    match (Array.length Sys.argv) with
      | 2 -> Some Sys.argv.(1)
      | _ -> None
  in
  let home = Unix.getenv "HOME" in
  let cells = Filename.concat home "cells" in
  match date with
    | Some date's -> Seq.gendirwalk do_dir ~date:date's cells
    | None        -> Seq.gendirwalk do_dir cells
 