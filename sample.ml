(* #require "pgocaml"
#load "percells.cmo" *)
open Printf

(* initially I thought I'd need to revert a buffer
  to parse a string into a number; in fact not,
  but why waste a good fun -- posted to codepad.org as well *)
(* http://codepad.org/I5sPOxrA *)
let revert_buffer ibuf =
  let len = (Buffer.length ibuf) in
  let obuf = Buffer.create len in
  let rec drop n =
    match n with
    | -1 -> Buffer.contents obuf
    | _  -> begin
      Buffer.add_char obuf (Buffer.nth ibuf n);
      drop (n-1)
    end in
  drop (len-1)


(* terminate on 1 is equivalent to calling with n-1 *)
let rec skip_n_spaces ic n =
  let c = input_char ic in
  match c with 
  | ' ' | '\t' | '\n' -> if n == 1 then () else skip_n_spaces ic (n-1)
  | _ -> skip_n_spaces ic n
      

(* NB: add EOF handling *)
let skip_n_words ic n =
  let in_word = ref false in
  let rec go ic n =
    let c = input_char ic in
    match c with
    | ' ' | '\t' | '\n' ->  
      if !in_word then 
        begin
          if n == 0 then () 
          else begin 
            in_word := false; 
            go ic (n-1) 
          end
        end
      else (* multiple white spaces *)
        go ic n
    | _ -> 
      begin 
        in_word := true; 
        go ic n
      end
    in
    if n <= 0 then () 
    else go ic (n-1)
      
      
let read_list1 ?skip ?n filename =
  let ic = open_in filename in begin
  match skip with
    | Some n -> skip_n_words ic n
    | None -> () 
  end;
  let buf = Buffer.create 10 in
  let rec parse ic buf len res =
    try
      let c = input_char ic in
      match c with
      | ' ' | '\n' -> if (Buffer.length buf) == 0 
        then parse ic buf len res 
        else let number = int_of_string (Buffer.contents buf) in
        begin
          Buffer.reset buf;
          let res = number::res in
          let len = len + 1 in
          match n with
          | Some n when len == n -> close_in ic; List.rev res
          | _ -> parse ic buf len res
        end
      |'0'..'9' -> 
        begin
          Buffer.add_char buf c;
          parse ic buf len res
        end
      | _ -> printf "[%c]\n" c; failwith "bad number file"
    with End_of_file -> close_in ic; List.rev res
    in
    parse ic buf 0 []


(* -- using DynArray, use extlib or batteries for that
let read_list2 ?skip ?n filename =
  let ic = open_in filename in begin
  match skip with
    | Some n -> skip_n_words ic n
    | None -> () 
  end;
  let buf = Buffer.create 10 in
  let rec parse ic buf len res =
    try
      let c = input_char ic in
      match c with
      | ' ' | '\n' -> if (Buffer.length buf) == 0 
        then parse ic buf len res 
        else let number = int_of_string (Buffer.contents buf) in
        begin
          Buffer.reset buf;
          DynArray.add res number;
          let len = len + 1 in
          match n with
          | Some n when len == n -> close_in ic; (DynArray.to_array res)
          | _ -> parse ic buf len res
        end
      |'0'..'9' -> 
        begin
          Buffer.add_char buf c;
          parse ic buf len res
        end
      | _ -> printf "[%c]\n" c; failwith "bad number file"
    with End_of_file -> close_in ic; (DynArray.to_array res)
    in
    parse ic buf 0 (DynArray.create ())
 *)

(* NB: add EOF handling *)
let rec skip_lines ic n =
  match n with
  | 0 -> ()
  | _ when n < 0 -> failwith "can only skip nonnegative number of lines"
  | _ -> ignore (input_line ic); skip_lines ic (n-1)
  

let read_lines ?skip ?n filename =
  let ic = open_in filename in
  begin
    match skip with
      | Some n -> skip_lines ic n
      | None -> ()
  end;
  let rec go ic len acc =
    match n with
    | Some n when len == n -> close_in ic; List.rev acc
    | _ ->
      try
        let line = input_line ic in
        go ic (len+1 )(line::acc)
      with End_of_file -> close_in ic; List.rev acc
  in
  go ic 0 []


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
  
  
let which_ith list x =
  let rec go l x i =
    match l with
    | x'::xs -> if x = x' then Some i else go xs x (i+1)
    | _ -> None
  in go list x 0
  

let range ?(from=1) upto =
  (* let range ?from upto = let from = match from with | Some x -> x | None -> 1 in *)
  let rec go from upto acc =
    if from > upto then acc else go from (upto-1) (upto::acc)
  in
  go from upto []
  
let range0 = range ~from:0
  
let matrix_column m i = Array.map (fun x -> x.(i)) m

(* find people who have enough of both training and test data *)

let ok_man ?(min=100) pput sample_len =
  let filter = function
  | (pos,man),(upto,total) when upto > min && total > min && total - upto >= sample_len -> true
  | _ -> false
  in
  List.filter filter pput

let percells_dataframe () =
  printf "fetching dataframe from database\n";
  let colnames,percells = Percells.fetch () in
  let colnames = List.map (function | Some x -> x | None -> "") colnames in
  let matrix = matrix_of_percells percells in
  colnames,matrix
  
let save_dataframe dataframe filename =
  let ob = open_out_bin filename in
  Marshal.to_channel ob dataframe [];
  close_out ob
  
let load_dataframe filename =
  let ib = open_in_bin filename in
  printf "loading dataframe from file %s\n" filename;
  let ((colnames: string list), (matrix: int array array)) = Marshal.from_channel ib in
  close_in ib;
  colnames,matrix
  
let get_dataframe ?fromfile () =
  match fromfile with
  | Some file -> load_dataframe file
  | None      -> percells_dataframe ()
  
let pput dataframe from sample_len = 
  (* let from = Sys.argv.(1) in *)
  (* select a person at random whose training and test sets are non-zero *)
  let colnames,matrix = dataframe in
  let date_colnum = match (which_ith colnames from) with
  | Some i -> i
  | None -> failwith "bad date" in
  let upto_date    = Array.to_list (matrix_column matrix date_colnum) in
  let total_colnum = match (which_ith colnames "total") with (* (List.length colnames)-1, as the last *)
  | Some i -> i
  | None -> failwith "bad totals" in
  let totals = Array.to_list (matrix_column matrix total_colnum) in
  let people = Array.to_list (matrix_column matrix 0) in (* (which_ith colnames "person_oid") *)
  let pos_people  = List.combine (range0 ((List.length people)-1)) people in
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

let pick_start our_guy sample_len =
  let _,(upto,total) = our_guy in
  let end_starts = upto + sample_len in
  let start = upto + Random.int (total - end_starts) (* +1? nah? *) in
  start
  
let sample our_guy start from sample_len =
  let (pos,person_oid),(upto,total) = our_guy in
  let prefix_len = start - 1 in
  let finish = prefix_len + sample_len in
  printf "person_oid: %d, skipping %d, upto = %d, total = %d\n" person_oid prefix_len upto total;
  let cells  = read_list1 ~skip:prefix_len ~n:sample_len (person_cells_file person_oid) in
  let times  = read_lines ~skip:prefix_len ~n:sample_len (person_times_file person_oid) in
  (cells,times),our_guy,(start,finish)

  (*
  let s1 = sample "2004-10-01" 10 
  *)
  
let print_list ?(out=stdout) list =
  let list's = String.concat "\t" list in
  output_string out list's;
  output_string out "\n"


(* picking apart a row of eligible, or person_info below *)
let person_oid   = function (_,x),_   -> x

(* picking apart a sample *)
let sample_cells = function (x,_),_,_ -> x

let sample_times = function (_,x),_,_ -> x

let person_info  = function _,(x,_),_ -> x

let upto_total   = function _,(_,x),_ -> x

let start_finish = function _,(_,_),x -> x

let write_int_list ilist filename =
  let oc = open_out filename in
  print_list ~out:oc (List.map string_of_int ilist);
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
  
let person_rank (oid:int) oids =
  let last = (List.length oids) - 1 in
  let oids_nth = List.combine oids (range0 last) in
  List.assoc oid oids_nth

let find_rank person_oid ares =
  (* Evalm.print_perps ares *)
  let lres = Array.to_list ares in
  let rank = person_rank person_oid (List.map (function (x,_,_) -> x) lres) in
  printf "person_oid %d => rank %d!\n" person_oid rank;
  person_oid,rank

let rank_person_file cells from sample_list_filename person_oid =
  (* we'll keep the LM results as both array and list, for convenience *)
  let ares = Evalm.evalaway_file cells (Some from) sample_list_filename in
  find_rank person_oid ares

let rank_person_serv (link:bool) (from:string) (person_ports: (int * int) list) (sample_list_filename:string) (person_oid:int) =
  (* we'll keep the LM results as both array and list, for convenience *)
  let ares = Evalm.evalaway_serv link from person_ports sample_list_filename in
  find_rank person_oid ares
  
let rec take l n =
  let rec go l n acc =
  match l with
  | x::xs when n > 0 -> go xs (n-1) (x::acc) 
  | _ -> List.rev acc in
  go l n []
  
  
let print_results ?(oc=stdout) ranks =
  List.iter (List.iter (function oid,rank -> fprintf oc "%d\t%d\n" oid rank)) ranks;
  close_out oc
  
let write_results ranks filename =
  let oc = open_out filename in
  print_results ~oc:oc ranks;
  close_out oc

let issome = function Some _ -> true | _ -> false
(* let issome x = x <> None *)
let unsome = function Some x -> x | None -> assert false
  
(* can this be done with one Pcre.<exec-type-op>?: *)
let elem_match list regex's =
  let re = Pcre.regexp regex's in
  try
    let elem = List.find (fun x -> Pcre.pmatch ~rex:re x) list in
    let meat = (Pcre.extract ~rex:re elem).(1) in
    Some meat
  with Not_found -> None
  
let yes_no = function | true -> "yes" | _ -> "no"

let () =
  let argv = Array.to_list Sys.argv in
  let order = 5 in (* parameterize *)
  let from_opt = elem_match argv "--from=(\\d{4}(-\\d{2}){2})" in
  let from = match from_opt with 
  | Some date's -> date's
  | None -> failwith "we need a from date for naming samples and results" in
  let sample_len = match (elem_match argv "--sample=(\\d+)") with
  | Some len's -> int_of_string len's
  | None -> 10 in
  let percells_file = elem_match argv "--matrix=(.*\\.bin)" in
  let dataframe = match percells_file with
  | Some file -> get_dataframe ~fromfile:file ()
  | None      -> get_dataframe () in
  let each_person_runs = match (elem_match argv "--runs=(\\d+)") with
  | Some runs -> int_of_string runs
  | None -> 1 in
  let take_people = elem_match argv "--take=(\\d+)" in
  let ppp_opt = elem_match argv "--ppp=(.*\\.ppp)" in
  let using_servers = ppp_opt <> None in
  let link = elem_match argv "(--clients)" <> None in
  let person_ports = match ppp_opt with
    | Some filename -> begin let ppp = Evalm.read_ppp filename in 
                       let pp = List.map (function x,y,_ -> x,y) ppp in
                       match link with
                       | true -> Evalm.create_all_clients order pp
                       | _    -> pp end
    | None          -> []
  in
  let using_what = if using_servers 
  then sprintf "servers (from %s), clients = %s" (unsome ppp_opt) (yes_no link)
  else "files" in
  printf "evaluating on trained before %s using %s\n" from using_what;
  
  let home   = Unix.getenv "HOME" in
  let cells  = Filename.concat home "cells" in
  let inputs = Filename.concat cells "input" in
  let sample_filebase = Filename.concat inputs "sample" in
  let sample_list_base = sample_filebase ^ "-list" in
  let sample_info_base = sample_filebase ^ "-info" in
  
  (* List.iter (fun i ->
    let observed = sample eligible from sample_len in
    print_sample observed) [1;2;3]
  *)

  let eligible = pput dataframe from sample_len in
  
  let sample_index = 3 in (* use sample_len to differentiate *)
  let sample_suffix = 
          "-" ^ from 
        ^ "-" ^ (string_of_int sample_len) 
        ^ "-" ^ (string_of_int sample_index) in
  let sample_list_filename = sample_list_base ^ sample_suffix in
  let sample_info_filename = sample_info_base ^ sample_suffix in
  let ranks = Filename.concat cells "ranks" in
  let ranks_base = Filename.concat ranks "ranks" in
  let ranks_filename = ranks_base ^ sample_suffix in

  let some_people = match take_people with
  | Some few -> take eligible (int_of_string few)
  | None -> eligible in
  
  let ranks = List.map
  (fun person -> List.map 
    (fun i ->
      let i's = string_of_int i in
      let start = pick_start person sample_len in
      let oid   = person_oid person in
      let oid's = string_of_int oid in
      let observed = sample person start from sample_len in
      let case_suffix = "-p"^oid's^"-"^i's in
      let case_list_filename = sample_list_filename ^ case_suffix in
      let case_info_filename = sample_info_filename ^ case_suffix in
      write_int_list (sample_cells observed) case_list_filename;
      write_sample observed from             case_info_filename;
      
      if using_servers then
        rank_person_serv link  from person_ports case_list_filename oid
      else
        rank_person_file cells from              case_list_filename oid
        
      ) (* fun i *)
      
        (range each_person_runs))
      some_people
  in
  
  print_results ranks;
  write_results ranks ranks_filename;
  
  if link then Evalm.destroy_all_clients person_ports else ();
  
  (* NB add time period for sample and test span coverage *)
  (* NB people clustering by top rank *)
  (* NB error as a function of test-training time distance *)
  (* NB compressing all repeated cells; skip LM models?  FLM? *)
  (* NB retain only small sample info (starting point seq number -- devise SQL to get row from person's seq) *)