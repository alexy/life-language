open Printf
open Utils
open Lmclass
open Baseclient

(* given a sample, cut of a suffix, generate a bunch of new ones,
  and measure how well we predict the actual *)

let generate ?(n=1) lm len seq =
  let seqlen = List.length seq in
  if len >= seqlen then (seq,[]) else
  let preflen = seqlen-len in
  let prefix,suffix = take preflen seq,
                      drop len seq in
  printf "giving prefix [|%s|] and length %d\n" (join prefix) seqlen;
  
  let suffixes = List.map (fun i ->
    let news = lm#complete_sentence seqlen prefix in
    drop preflen news) (range n)
  in
  (prefix,suffixes)
  
let generate_many n lm len seqs =
  List.map (generate ~n lm len) seqs
    
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
  (* -- before vocab.read(), I noticed that
     -- a computation has to be made to get some real words
     -- besides -pau- -pau- -pau- ...
  let ppl = lm#compute seqfile in 
  printf "seq compute =>\n%s\n\n" ppl; *)
  printf "reading sample from %s\n" seqfile;
  let seqs = Seq.read_many seqfile in
  (* let seqs = [[1;2;3;4;5;6;7;8;9;10];[10;9;8;7;6;5;4;3;2;1]] in *)
  
  let pereach = 3 in
  let gens = generate_many pereach lm len seqs in
  List.iter2 (fun seq (prefix,suffixes) ->
    printf "original sentence: %s\n" (join seq);
    printf "prefix %d words: %s\n" (List.length prefix) (join prefix);
    printf "generated +%d words, %d times:\n" len (List.length suffixes);
    List.iter (fun suffix -> printf "  :%s\n" (join suffix)) suffixes) seqs gens;
