(* OCaml bindings for SRILM
   Copyright (c) Alexy Khrabrov <deliverable@gmail.com>, 2008.
   Released under GPLv3 -- see FSF.org for details *)

open Printf
open Lmclass

let () =
  print_endline "let's go";

  let portbase = 10000 in
  let vocab = "/Users/alexyk/cells/vocab/all-cells.txt" in
  let lms = List.map (fun i ->
    let port = portbase + i in
    let lm = new lmclient (sprintf "%d@localhost" port) 5 vocab in
    printf "handle = %d\n" lm#int_handle; flush stdout;
    let results = lm#compute "/Users/alexyk/cells/seq40" in
    Printf.printf "=> %s\n" results;
    lm) [8;3]
  in
  (* NB is there a way to do something like,
    (List.map lmclient#int_handle lms)
    *)
  printf "handles for the lm list are in fact: ";
  print_endline (String.concat ", " (List.map (fun lm -> string_of_int lm#int_handle) lms));

  let lm0 = List.hd lms in
  let prefix = [1;2;3] in (* ["1";"2";"3"] *)
  let sentence = lm0#complete_sentence 10 prefix in
  let sentence = List.map string_of_int sentence in
  let sentence's = String.concat " " sentence in
  printf "sentence => %s\n" sentence's;
  
  List.iter (fun lm ->
    let num = lm#destroy in
    printf "num = %d\n" num) (List.rev lms)
