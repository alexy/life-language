open Printf

let read_process command =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer

let wordcount file =
  let wc_w = read_process ("wc -w "^file) in
  let w's = List.hd (Str.split_delim (Str.regexp " ") wc_w) in
  int_of_string w's
  
let dir_lm ?mincount ?date dir =
  let mincount = match mincount with Some x -> x | None -> 10 in
  let filename = Filename.basename dir in
  let filename = filename ^ (match date with Some date -> "-"^date | None -> "") in
  Unix.chdir dir;
  let nwords = wordcount filename in
    if nwords < mincount then
      printf "not enough words in %s, only %d -- skipping...\n" dir nwords
    else
      let command1 = sprintf "ngram-count -order 5 \
        -text %s -sort -write %s.counts" filename filename in
      print_endline command1;
      ignore (Unix.system command1);
      let command2 = sprintf "ngram-count -order 5 \
        -vocab ~/cells/vocab/all-cells.txt -lm mitr-wb5-%s.lm \
        -wbdiscount -read %s.counts" filename filename in
      (* print_endline command2; *)
      ignore (Unix.system command2)
  
(* dir_lm "/Users/alexyk/cells/1" *)
    

let () =
  let date = 
    match (Array.length Sys.argv) with
      | 2 -> Some Sys.argv.(1)
      | _ -> None
  in
  let home = Unix.getenv "HOME" in
  let cells = Filename.concat home "cells" in
  match date with
    | Some date's -> Seq.gendirwalk dir_lm ~date:date's cells
    | None        -> Seq.gendirwalk dir_lm cells
    
(* NB should we use vocab as union of all cells not for all time, but in the training span, and map unseens to unk? *)