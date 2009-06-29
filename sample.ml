(* #require "pgocaml"
#load "percells.cmo" *)
open Printf
open Baseclient


let list_of_tuple tuple =
  let list = Array.to_list (Obj.magic tuple) in
  let head = (Obj.magic (List.hd list) : int32)
  and tail = (Obj.magic (List.tl list) : int32 option list) in
  (Some head)::tail


let matrix_of_percells percells =  
  let nrows = List.length percells in
  let ncols = List.length (list_of_tuple (List.hd percells)) in
  let matrix = Array.make_matrix nrows ncols 0 in
  let rec rows ll row =
    let rec cols l col =
      match l with
      | x'::xs -> 
        begin
          let x =
            match x' with
              | Some x -> Int32.to_int x
              | None -> 0
            in
            matrix.(row).(col) <- x;
            cols xs (col+1)
        end
      | _ -> ()
    in
    match ll with
      | y::ys -> begin cols (list_of_tuple y) 0; rows ys (row+1) end
      | _ -> ()
    in
    rows percells 0;
    matrix

(* matrix_of_percells percells *)


let matrix_of_percells_too percells =
  let nrows = List.length percells in
  let ncols = List.length (list_of_tuple (List.hd percells)) in
  let matrix = Array.make_matrix nrows ncols 0 in
  Array.iteri (fun i y -> Array.iteri (fun j x -> matrix.(i).(j) <- match x with Some x -> Int32.to_int x | None -> 0) 
      (Array.of_list (list_of_tuple y))) 
    (Array.of_list percells);
  matrix
  
    
let matrix_column m i = Array.map (fun x -> x.(i)) m

(* find people who have enough of both training and test data *)

let ok_man ?(min=100) pput sample_len =
  let filter = function
  | (pos,man),(upto,total) when upto > min && total > min && total - upto >= sample_len -> true
  | _ -> false
  in
  List.filter filter pput

  
let pput dataframe from sample_len = 
  (* let from = Sys.argv.(1) in *)
  (* select a person at random whose training and test sets are non-zero *)
  let colnames,matrix = dataframe in
  let date_colnum = match (Utils.which_ith colnames from) with
  | Some i -> i
  | None -> failwith "bad date" in
  let upto_date    = Array.to_list (matrix_column matrix date_colnum) in
  let total_colnum = match (Utils.which_ith colnames "total") with (* (List.length colnames)-1, as the last *)
  | Some i -> i
  | None -> failwith "bad totals" in
  let totals = Array.to_list (matrix_column matrix total_colnum) in
  let people = Array.to_list (matrix_column matrix 0) in (* (which_ith colnames "person_oid") *)
  let pos_people  = List.combine (Utils.range0 ((List.length people)-1)) people in
  let upto_totals = List.combine upto_date totals in
  (* PosPeopleUpTo => pput: *)
  let pput        = List.combine pos_people upto_totals in
  ok_man pput sample_len
  
(* let r = pput dataframe "2004-10-01" 100  *)

(* NB factor out person_dir/*_file for reuse in evalm and elsewhere: *)
let person_dir id =
  let home = Unix.getenv "HOME" in
  let cells = Filename.concat home "cells" in
  Filename.concat cells (string_of_int id)
  
let person_cells_file id =
  Filename.concat (person_dir id) (string_of_int id)

let person_times_file id =
  Filename.concat (person_dir id) "times-"^(string_of_int id)
  
let pick_person eligible =
  let eligible_len = List.length eligible in
  let i = Random.int eligible_len in
  List.nth eligible i

let pick_start sample_len our_guy =
  let _,(upto,total) = our_guy in
  let end_starts = upto + sample_len in
  let start = upto + Random.int (total - end_starts) (* +1? nah? *) in
  start
  
let pick_starts sample_len our_guy n =
  (* call a fun n times -- independent of iteration number! *)
  List.map (fun _ -> pick_start sample_len our_guy) (Utils.range n)
  
let sample ?times from sample_len our_guy start =
  let (pos,person_oid),(upto,total) = our_guy in
  let prefix_len = start - 1 in
  let finish = prefix_len + sample_len in
  (* printf "person_oid: %d, skipping %d, upto = %d, total = %d\n" person_oid prefix_len upto total; *)
  let cells  = Seq.read_cell_line ~skip:prefix_len ~n:sample_len (person_cells_file person_oid) in
  (* NB times are single lines-column file per sample, while sample now is batched multiline;
     -- therefore we need to figure out a way to stire/read times differently in a single file *)
  let times  = match times with
  | Some _ -> Seq.read_lines ~skip:prefix_len ~n:sample_len (person_times_file person_oid)
  | None -> [] in
  (cells,times),our_guy,(start,finish)
  
let samples from sample_len our_guy starts =
  List.map (sample from sample_len our_guy) starts
  
let print_list ?(out=stdout) list =
  let list's = String.concat "\t" list in
  output_string out list's;
  output_string out "\n"


(* picking apart a row of eligible, or person_info below *)
let person_oid   = function (_,x),_   -> x

(* picking apart a sample *)
let sample_cells = function (x,_),_,_ -> x
let sample_cells_list = List.map sample_cells

let sample_times = function (_,x),_,_ -> x

let person_info  = function _,(x,_),_ -> x

let upto_total   = function _,(_,x),_ -> x

let start_finish = function _,(_,_),x -> x

let write_int_list list filename =
  let oc = open_out filename in
  print_list ~out:oc (List.map string_of_int list);
  close_out oc

let write_int_lists lists filename =
  let oc = open_out filename in
  List.iter (fun list -> print_list ~out:oc (List.map string_of_int list)) lists;
  close_out oc

let print_sample ?(out=stdout) ?full observed date =
  let person_row,person_oid = person_info observed in
  match full with
  | Some _ -> (* NB what shortest actual param do you give as "any"? *)
    print_list ~out:out (List.map string_of_int (sample_cells observed));
    print_list ~out:out (sample_times observed);
    fprintf out "person_oid: %d (row %d)" person_oid person_row;
    let upto,total = upto_total observed in
    fprintf out "\tupto %s: %d (total %d)" date upto total;
    let start,finish = start_finish observed in
    fprintf out "\tstart %d (finish %d)\n" start finish
  | None -> 
    let start,_ = start_finish observed in
    fprintf out "%d\n" start

let write_sample sample date filename =
  let oc = open_out filename in
  print_sample ~out:oc sample date;
  close_out oc
  
let write_samples samples date filename =
  let oc = open_out filename in
  List.iter (fun sample -> (print_sample ~out:oc sample date)) samples;
  close_out oc
  
  
let person_rank (oid:int) oids =
  let last = (List.length oids) - 1 in
  let oids_nth = List.combine oids (Utils.range0 last) in
  List.assoc oid oids_nth

let array_find ex array elem =
  let last = (Array.length array) - 1 in
  let rec go i =
    if ex array.(i) = elem then Some i
    else if i < last then go (i+1)
    else None
  in
  go 0
      
let triple_person = function (x,_,_) -> x
let triple_person's = function x -> string_of_int (triple_person x)

let find_rank ?(infinite=1000) person_oid ares =
  (* Evalm.print_perps ares *)
  (* NB: instead of this, find in array via binary search! *)
  let ranko = array_find triple_person ares person_oid in
  match ranko with
  | Some rank ->
    printf "person_oid %d => rank %d!\n" person_oid rank; flush stdout;
    (* printf "list: %s\n" (String.concat "," (List.map triple_person's (Array.to_list ares)));*) (* NUM *)
    person_oid,rank
  | None -> printf "*** person_oid %d *** rank not found\n" person_oid;
    printf "list: %s\n" (String.concat "," (List.map triple_person's (Array.to_list ares)));
    person_oid,infinite (* a really big rank, bigger than for real *)
  
let find_ranks person_oid lares =
  List.map (find_rank person_oid) lares

let case_filenames base_filenames each_person_runs person run =
  let (sample_list_filename,sample_info_filename) = base_filenames in
  let oid = person_oid person in
  let run's = string_of_int run in
  let oid's = string_of_int oid in
  let case_suffix = "_p"^oid's
    ^ ( if each_person_runs > 1 then "-"^run's else "") in
  let case_list_filename = sample_list_filename ^ case_suffix in
  let case_info_filename = sample_info_filename ^ case_suffix in
  (case_list_filename,case_info_filename)
  
let generate_person_batch from sample_len batch case_filenames person =
  let (case_list_filename,case_info_filename) = case_filenames in
  let starts = pick_starts sample_len person batch in
  let observed = samples from sample_len person starts in
  write_int_lists (sample_cells_list observed) case_list_filename;
  write_samples   observed from                case_info_filename
  
let rank_person_file cells from sample_list_filename person_oid =
  (* we'll keep the LM results as both array and list, for convenience *)
  let lares = Evalm.evalaway_file cells (Some from) sample_list_filename in
  find_ranks person_oid lares

let rank_person_serv person_ports link from sample_list_filename person =
  (* we'll keep the LM results as both array and list, for convenience *)
  (* diversify eval walk order for parallel setups: *)
  let oid = person_oid person in
  let pp = if (*true*) oid mod 2 = 1 then (* NUM don't have to revert; shift *)
    begin
      printf "original order for %d, samples from %s\n" oid sample_list_filename; 
      person_ports 
    end  
  else 
    begin
      printf "reverting for %d" oid;      
      List.rev person_ports
    end
  in
  let lares = Evalm.evalaway_serv pp link from sample_list_filename in
  find_ranks oid lares
  
let take = Utils.take
  
let print_results ?(oc=stdout) ranks =
  List.iter (* i -- each_person_runs *)
    (List.iter (* batch -- several sequences per sample file *)
      (List.iter (* rank for each person *)
        (function oid,rank -> fprintf oc "%d\t%d\n" oid rank))) ranks;
  close_out oc
  
let write_results ranks filename =
  let oc = open_out filename in
  print_results ~oc:oc ranks;
  close_out oc

let issome = function Some _ -> true | _ -> false
(* let issome x = x <> None *)
let unsome = function Some x -> x | None -> assert false
    
let yes_no = function | true -> "yes" | _ -> "no"

(* NB: we have to BOTH provide upclass coercion below AND in evalm when creating person_ports lists! 
  Check this and try to relax...
  *)

  
let opt = Utils.elem_match
let rematch = Utils.rematch
let join = Utils.join

let upperclass_clients =
  List.map (function port,client -> (port, (client :> baseclient)))

let () =
  let argv = Array.to_list Sys.argv in
  let threads = match (opt argv "--threads=(\\d+)") with
  | Some i -> int_of_string i
  | None -> 2 in
  let order = match (opt argv "--order=(\\d+)") with
  | Some i -> int_of_string i
  | None -> 5 in
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
  let sample_len = match (opt argv "--sample=(\\d+)") with
  | Some len's -> int_of_string len's
  | None -> 10 in
  (* let vocab = match (opt argv "--vocab=(.+)") with
  | Some file -> file
  | None -> "/Users/alexyk/cells/vocab/all-cells.txt" in *)
  let percells_file = opt argv "--matrix=(.+\\.bin)" in
  let dataframe = match percells_file with
  | Some file -> Dataframe.get ~fromfile:file ()
  | None      -> Dataframe.get ~fromfile:"percells.bin" () (* Dataframe.get () *) in
  let each_person_runs = match (opt argv "--runs=(\\d+)") with
  | Some runs -> int_of_string runs
  | None -> 1 in
  let batch = match (opt argv "--batch=(\\d+)") with
  | Some batch -> int_of_string batch
  | None -> 1 in
  let batch_reuse = opt argv "(--reuse)" <> None in
  let suffix = match (opt argv "--suffix=(\\d+)") with
  | Some s -> int_of_string s
  | None -> 5 in
  let take_people = opt argv "--take=(\\d+)" in
  (* let arg_person = match (opt argv "--person=(\\d+)") with
  | Some n -> Some (int_of_string n)
  | None -> None in *)
  let link = opt argv "(--clients)" <> None in
  let parallel = opt argv "(--parallel)" <> None in
  (* parameterize home, cells, vocab *)
  let home   = Unix.getenv "HOME" in
  let cells  = Filename.concat home "cells" in
  let vocab  = List.fold_left Filename.concat cells ["vocab";"all-cells.txt"] in
  let person_ports = match ppp_opt with
    | Some filename -> begin let ppp = Common.read_ppp filename in 
                       let pp = List.map (function x,y,_ -> x,y) ppp in
                       match link with
                       | true -> upperclass_clients (Evalm.create_all_clients ~init:(not parallel) ~vocab order pp)
                       | _    -> upperclass_clients (Evalm.create_all_systems order pp)
                       end
                       (* the range below can be obtained from examining cells/ *)
    | None          -> upperclass_clients (Evalm.create_all_commands from_opt order (Utils.range 100)) in
  let using_what = if using_servers 
  then sprintf "servers (from %s), clients = %s, batch_reuse = %s" (unsome ppp_opt) (yes_no link) (yes_no batch_reuse)
  else "files" in
  printf "evaluating on trained before %s using %s\n" from using_what;
  
  printf "initializing random number generator";
  Random.self_init ();
 
  (* parameterize inputs, samples *)
  let sample_index = suffix in (* use sample_len to differentiate *)
  let sample_suffix = 
          "_" ^ from 
        ^ "_s" ^ (string_of_int sample_len)
        ^ (if batch > 1 then "_x" ^ (string_of_int batch) else "")
        ^ "-" ^ (string_of_int sample_index) in

  let samples_base = "samples" in
  let samples_dirname = "samples" ^ sample_suffix in
  let inputs = List.fold_left Filename.concat cells [samples_base;samples_dirname] in

  let sample_filebase = Filename.concat inputs "sample" in
  let sample_list_base = sample_filebase ^ "-list" in
  let sample_info_base = sample_filebase ^ "-info" in  

  let sample_list_filename = sample_list_base ^ sample_suffix in
  let sample_info_filename = sample_info_base ^ sample_suffix in
  let sample_base_filenames = (sample_list_filename,sample_info_filename) in

  let ranks = Filename.concat cells "ranks" in
  let ranks_base = Filename.concat ranks (sprintf "ranks-%dg" order) in
  let ranks_filename = ranks_base ^ sample_suffix in
  
  if Utils.directory_exists inputs then ()
  else begin
    if not batch_reuse then Unix.mkdir inputs 0o755
    else failwith "cannot reuse non-existing samples"
  end;
  
  (* List.iter (fun i ->
     let observed = sample eligible from sample_len in
     print_sample observed) [1;2;3]
  *)

  let eligible = pput dataframe from sample_len in
  let some_people,say = match take_people with
  | Some few's -> let few = int_of_string few's in
                  take few eligible,"some"
  | None -> eligible,"all" in
  let the_people  = List.map person_oid some_people in
  let person_ports = List.filter (fun pp ->
    let person = fst pp in
    List.mem person the_people
    ) person_ports
  in
  printf "processing %s %d eligible people, %d person-ports\n"
    say (List.length some_people) (List.length person_ports);
  let ports_people     = List.map fst person_ports in
  printf "those people: %s\n" (join the_people);
  printf "ports people: %s\n" (join ports_people);
  
  let the_map = if parallel then
    Parallel.pmap_init ~process_count:threads (fun l ->            
        Evalm.init_all_clients person_ports)
        
  else List.map in
   
  let ranks = 
    the_map
    (fun person -> 
      let res = List.map 
      (fun run ->
        let filenames = case_filenames sample_base_filenames each_person_runs person run in
        if batch_reuse then ()
        else generate_person_batch from sample_len batch filenames person;
        rank_person_serv person_ports link from (fst filenames) person
        ) 
      (Utils.range each_person_runs) in 
      res)
    some_people
  in
  
  print_results ranks; flush stdout;
  write_results ranks ranks_filename;
  
  if link && not parallel then Evalm.destroy_all_clients person_ports else ();
  
  (* NB add time period for sample and test span coverage *)
  (* NB people clustering by top rank *)
  (* NB error as a function of test-training time distance *)
  (* NB compressing all repeated cells; skip LM models?  FLM? *)
  (* NB retain only small sample info (starting point seq number -- devise SQL to get row from person's seq) *)
  (* NB compute time range for each sample, write out in full sample *)