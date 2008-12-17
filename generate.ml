open Printf
open Utils
open Lmclass
open Baseclient

(* given a sample, cut of a suffix, generate a bunch of new ones,
  and measure how well we predict the actual *)

let generate lm seq len =
  let seqlen = List.length seq in
  if len >= seqlen then [] else
  let prefix,suffix = take (seqlen-len) seq,
                      drop len seq in
  printf "giving prefix [|%s|] and length %d\n" (join prefix) seqlen;
  let news = lm#complete_sentence seqlen prefix in
  news
  
let create_client order port vocab =
    let port's = sprintf "%d@localhost" port in
    printf "creating client for %s\n" port's;
    let client =  new lmclient port's order vocab in
    assert (client#int_handle > 0);
    client (* :> baseclient *)

let opt = Utils.elem_match

let () =
  let argv = Array.to_list Sys.argv in
  let len = match (opt argv "--len=(\\d+)") with
  | Some is -> int_of_string is
  | None -> 5 in
  let order = match (opt argv "--order=(\\d+)") with
  | Some is -> int_of_string is
  | None -> 5 in
  let person_prefix = "--person=" in
  let person = match (opt argv (person_prefix^"(\\d+)")) with
  | Some is -> int_of_string is
  | None -> failwith ("need a person with "^person_prefix) in
  let seq_prefix = "--seq=" in
  let seqfile = match (opt argv (seq_prefix^"([^ ]+)")) with
  | Some file -> file
  | None -> failwith ("need a sequence file with "^seq_prefix) in
  let ppp_prefix = "--ppp=" in
  let ppp_file = match (opt argv (ppp_prefix^"(.*\\.ppp)")) with
  | Some file -> file
  | None -> failwith ("need the ppp file with "^ppp_prefix) in
  let ppp = Common.read_ppp ppp_file in
  let person_ports = List.map (function x,y,_ -> x,y) ppp in
  
  let our_pp =
  try
    List.find (fun (p,_) -> p = person) person_ports
  with Not_found -> failwith "this person is not modeled by our servers" in
  
  let vocab = "/Users/alexyk/cells/vocab/all-cells.txt" in
  let port = snd our_pp in
  let lm = create_client order port vocab in
  let ppl40 = lm#compute "/Users/alexyk/cells/seq40" in
  printf "seq40 compute =>\n%s\n\n" ppl40;
  printf "reading sample from %s\n" seqfile;
  let seq = Seq.read_cells seqfile in
  (* let seq = [1;2;3;4;5;6;7;8;9;10] in *)
  let news = generate lm seq len in
  printf "original prefix: %s\n" (join seq);
  printf "generated -+%d words: %s\n" len (join news) (* (String.concat " " news') *)