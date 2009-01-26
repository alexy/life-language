open Printf
open Utils

let report what ?date ppp =
  let maybe_date = match date with 
  | Some date -> Printf.sprintf " (trained before %s)" date
  | None      -> "" in
  let total = List.length ppp in
  let from_port,to_port = Evalm.port_range ppp in
  Printf.printf "\n%s %d servers%s on ports %d-%d\n" what total maybe_date from_port to_port

type control = Start | Stop

let opt = Utils.elem_match

let () =
  let argv = Array.to_list Sys.argv in
  
  let start = (opt argv "(^start$)") <> None in
  let stop  = (opt argv "(^stop$)")  <> None in
  if not (stop || start) || (stop && start) then failwith "either stop or start" else ();
  let action = if start then Start else Stop in

  let order = match (opt argv "--order=(\\d+)") with
  | Some i -> int_of_string i
  | None -> 5 in
  let maxclients = match (opt argv "--maxclients=(\\d+)") with
  | Some i -> int_of_string i
  | None -> 2 in
  let port_base = match (opt argv "--base=(\\d+)") with
  | Some base's -> int_of_string base's
  | None -> if stop then 0 else
    failwith "we need a port base to start things" in
  printf "order => %d, maxclients => %d, port_base => %d\n" order maxclients port_base;
  
  let ppp_ext = "\\.ppp" in
  let ppp_opt = opt argv ("--ppp=(.*"^ppp_ext^")") in
  let using_servers = ppp_opt <> None in
  let ppp_filename = match ppp_opt with Some x -> printf "ppp file given: %s\n" x; x | None -> "" in
  
  let date_re = "(\\d{4}(-\\d{2}){2})" in
  let from_opt = opt argv ("--from="^date_re) in
  printf "hey!\n"; 
  let need_from () = failwith "we need a from date for naming samples and results" in
  let from = match from_opt with 
  | Some date's -> date's
  | None -> if using_servers then begin
    match rematch ppp_filename (date_re^ppp_ext) with
    | Some date's -> date's
    | None -> need_from ()
  end else need_from () in


  
  
  Printf.printf "life pattern language family of servers controller\n";
  
  match action with
  | Start ->
    (* NB make cmdline arg *)
    let home   = Unix.getenv "HOME" in
    let cells  = Filename.concat home "cells" in
    let ppp = match from_opt with
    | Some date -> Evalm.launch_all_servers order maxclients cells ~date:from port_base ppp_filename
    | None ->      Evalm.launch_all_servers order maxclients cells port_base ppp_filename
    in
    report "started" ~date:from ppp; flush stdout;

  | Stop ->
    let ppp  = Common.read_ppp ppp_filename in
    let pids = List.map (function _,_,x -> x) ppp in
    List.iter (fun pid -> ignore (Unix.kill pid 1)) pids;
    report "stopped" ppp
    ;

    (* various ways to delay -- unnecessary, children keep living on their own!
    ignore (input_line stdin)
    Unix.sleep 60 
    *)
