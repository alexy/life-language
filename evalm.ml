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


let ppl_stats result = 
    match Str.string_match (Str.regexp
      (* "[^\n]+\n" -- for single-list file and the full pplFile return of stats *)
      "\\([0-9]+\\) zeroprobs, logprob= \\([0-9.-]+\\) ppl= \\([0-9.]+\\) ppl1= \\([0-9.]+\\)") 
      result 0 with 
      | true ->
        let numbers = Array.of_list 
        (List.map (
          fun x -> float_of_string (Str.matched_group x result)) [1;2;3;4]) in
        Some numbers
      | _ -> None

let rec third = function _::_::a::xs -> a::(third xs) | _ -> []  
(* let third list = List.fold_left2 
     (fun acc a i -> if i mod 3 = 0 then a::acc else acc) list (range (List.length list)) *)

let ppl_lines text =
  let lines = Str.split (Str.regexp "\n") text in
  let every3rd = third lines in
  List.map ppl_stats every3rd 

(* echo "1 38 1 2 43 1 43" | ngram -lm mitr-wb-40.lm -debug 2 -ppl - *)
(* file /Users/alexyk/cells/input/seq40: 1 sentences, 7 words, 0 OOVs
   0 zeroprobs, logprob= -9.10564 ppl= 13.7469 ppl1= 19.9897 *)
let evalm_file dir ?date seqfile =
  let person's = Filename.basename dir in
  let person'date, date's = 
    match date with 
      | Some date -> person's^"-"^date, date
      | None -> person's, ""
      in
  let lm = sprintf "%s/mitr-wb5-%s.lm" dir person'date in
  if Sys.file_exists lm then begin
      let person = int_of_string person's in
      let command = sprintf "ngram -lm %s -debug 1 -ppl %s" lm seqfile in
      (* print_endline command; *)
      output_string stdout "."; flush stdout;
      let result = read_process command in
      (* print_endline result *)
      let stats_list = ppl_lines result in
      List.map (fun stats -> person, date's, stats) stats_list
  end
  else []


let evadirwalk (f:string -> ?date:string -> string -> (int * string * float array option) list) ?date root seqfile =
  let numbers = Str.regexp "^[0-9]+$" in
  let subdirs = Array.to_list (Sys.readdir root) in
  let subdirs = List.filter (fun x -> Str.string_match numbers x 0 && x <> "0") subdirs in
  (* let subdirs = ["9"] in *)
  match date with
    | Some date ->
      List.map (fun x -> f (Filename.concat root x) seqfile ~date:date) subdirs
    | None ->
      List.map (fun x -> f (Filename.concat root x) seqfile) subdirs


let print_perp = function
  | (person,date,Some [|zeroprobs;logprob;ppl;ppl1|]) -> 
    printf "  person %2d date %s zerops %g logp %g ppl %g ppl1 %g\n" person date zeroprobs logprob ppl ppl1
  | (person, date, None) -> printf "-- person %d date %s --\n" person date
  | _ -> ()
  
  
let compare_perps x y =
  match x,y with
  | (_,_,Some _),(_,_,None) -> -1
  | (_,_,None),(_,_,Some _) -> 1
  | (_,_,Some [|_;_;x'ppl;x'ppl1|]),(_,_,Some [|_;_;y'ppl;y'ppl1|]) ->
    if x'ppl <> y'ppl then compare x'ppl y'ppl else compare x'ppl1 y'ppl1
  | (x'person,_,None),(y'person,_,None) -> compare x'person y'person
  | _ -> 0 (* begin
      print_endline "ill-formed perps:";
      print_perps x;
      print_perps y;
      failwith "exiting..."
    end *)
  
let sort_perps l = 
  let a = Array.of_list l in
  Array.sort compare_perps a;
  a

let sort_perp_lists lili =
  List.map sort_perps lili
    
let rec transpose = function 
  | []::_ -> [] 
  | list -> List.map List.hd list :: transpose (List.map List.tl list)
  
let evalaway_file cells date seqfile  =
  let vres = match date with
    | Some date's -> evadirwalk evalm_file cells ~date:date's seqfile
    | None        -> evadirwalk evalm_file cells seqfile
  in
  let lres = transpose vres in 
  sort_perp_lists lres (* converts result list to array for sorting *)
  
let print_perps ares =
  print_endline ""; (* after the progress dots *)
  Array.iter print_perp ares

(* let () =
  let seqfile = Sys.argv.(1) in
  let date = 
    match (Array.length Sys.argv) with
      | 3 -> Some Sys.argv.(2)
      | _ -> None
  in
  let home = Unix.getenv "HOME" in
  let cells = Filename.concat home "cells" in
  let ares = evalaway cells date seqfile in
  print_perps ares *)

(* NB let's start a bunch of TCP servers on ports 10000..10000+N, and eval against them! *)

let person_port base oid =
  base + oid

(* command line ends with -server-port *)
let spawn_server_process command_line_prefix port =  
  let port's = string_of_int port in
  let args = Array.of_list (Str.split (Str.regexp " ") command_line_prefix) in
  let args = Array.append args [|port's|] in


  let devnull = "/dev/null" in
  let in_devnull  = open_in  devnull in
  let out_devnull = open_out devnull in
  (* let err_devnull = open_out devnull in *)
  let ic = in_devnull in let oc = out_devnull in let ec = oc (* err_devnull *) in

 
  (* let ic = stdin in let oc = stdout in ec = let stderr in *)
  let in_fd  = Unix.descr_of_in_channel  ic in
  let out_fd = Unix.descr_of_out_channel oc in
  let err_fd = Unix.descr_of_out_channel ec in
  (* print_endline (String.concat " || " (Array.to_list args)); *)
  Unix.create_process args.(0) args in_fd out_fd err_fd


let launch_lm_server dir ?date port_base oc_ppp =
  let person's = Filename.basename dir in
  let person   = int_of_string person's in
  let (person'date, date's) = 
    match date with 
      | Some date -> (person's^"-"^date, date)
      | None -> (person's,"") 
    in
  let lm = sprintf "%s/mitr-wb5-%s.lm" dir person'date in
  if Sys.file_exists lm then 
    begin
      let command_line_prefix = sprintf "ngram -lm %s -order 5 -server-port" lm in
      let port = person_port port_base person in
      (* print_endline command_line_prefix; *)
      let pid = spawn_server_process command_line_prefix port in
      fprintf oc_ppp "%d\t%d\t%d\n" person port pid;
      output_string stdout ":"; flush stdout;        
      Some (person,port,pid) (* NB need parens, compare: Some 1,2 : int option * int ! *)
    end 
  else
    None
      
      
let launch_all_servers ?(f=launch_lm_server) root ?date port_base ppp_filename =
  let oc_ppp = open_out ppp_filename in
  let numbers = Str.regexp "^[0-9]+$" in
  let subdirs = Array.to_list (Sys.readdir root) in
  let subdirs = List.filter (fun x -> Str.string_match numbers x 0 && x <> "0") subdirs in
  (* let subdirs = ["1";"9"] in *)
  let opt_ppp = match date with
    | Some date ->
      List.map (fun x -> f (Filename.concat root x) ~date:date port_base oc_ppp) subdirs
    | None ->
      List.map (fun x -> f (Filename.concat root x) port_base oc_ppp) subdirs
  in
  close_out oc_ppp;
  let some_ppp = List.filter (function | Some _ -> true | None -> false)      opt_ppp  in
  let ppp = List.map (function | Some x -> x | _ -> failwith "can't be None") some_ppp in
  ppp (* person,port,pid *)
  

let ppp_port = function _,x,_ -> x

let rec some_last = function
  | x'::x::xs -> some_last (x::xs)
  | x::[] -> Some x
  | _ -> None
  
let last l = match some_last l with
| Some x -> x
| None -> raise (Failure "last") (* after List.hd -> Failure "hd" *)
  
let port_range ppp =
  let ports = List.map ppp_port ppp in
  let from = List.hd ports in
  let upto = last ports in
  from,upto
  
let read_ppp filename =
  let ic = open_in filename in
  let rec go ic acc =
    try 
      let line = input_line ic in
      let ppp = Scanf.sscanf line "%d %d %d" (fun x y z-> x,y,z) in
      go ic (ppp::acc)
    with End_of_file -> List.rev acc
  in
  go ic []
  
  
let evalm_serv date seqfile person_port =
  let person,port = person_port in
  let command = sprintf "ngram -use-server %d@localhost -ppl %s" port seqfile in
  (* print_endline command; *)
  let result = read_process command in
  (* print_endline result; *)
  output_string stdout "."; flush stdout;
  let stats_list = ppl_lines result in
  List.map (fun stats ->
  person, date, stats) stats_list
      

let evaportwalk f date person_ports seqfile =
  List.map (f date seqfile) person_ports
  (*  [[(person1,date,stats11);(person1,date,stats12);...;(person1,date,stats1N)];
       [(person2,date,stats21);(person2,date,stats22);...;(person2,date,stats2N)];
       ...;
       [(personM,date,statsM1);(personM,date,statsM2);...;(personM,date,statsMN)]];
       
       transpose ->
      [[(person1,date,stats11);...;(person2,date,stats21);...;(personM,date,statsM1)];
       [(person1,date,stats12);...;(person2,date,stats22);...;(personM,date,statsM2)];
       ...;
       [(person1,date,stats1N);...;(person2,date,stats2N);...;(personM,date,statsMN)];
      ]
   *)

let evalm_link date seqfile person_client =
  let person,client = person_client in
  let result = Lmclient.compute client seqfile in
  (* print_endline result; *)
  output_string stdout "."; flush stdout;
  let stats_list = ppl_lines result in
  List.map (fun stats ->
  person, date, stats) stats_list
  (*  [(personX,date,statsX1);(personX,date,statsX2);...;(personX,date,statsXN)] *)

let evalaway_serv person_ports link date seqfile  =
  let f = match link with
    | true -> evalm_link
    | _    -> evalm_serv in
  let vres = evaportwalk f date person_ports seqfile in
  let lres = transpose vres in 
  sort_perp_lists lres (* converts result list list to list array from sorting *)

let create_all_clients order person_ports =
  List.map (function person,port -> 
    let port's = sprintf "%d@localhost" port in
    let client = Lmclient.create port's order in
    (* if client < 0 then failwith "coulnd't create client" else (); *)
    assert (client >= 0);
    person,client) person_ports
    
let destroy_all_clients person_clients =
  let clients = List.map snd person_clients in
  let backwards = List.rev clients in
  List.iter (fun client -> assert ((Lmclient.destroy client) >= 0)) backwards
