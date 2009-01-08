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
    
    
let read_cells ?(skip=None) ?(n=None) ic =
  begin match skip with
    | Some n -> skip_n_words ic n
    | None -> ()
  end; 
  let buf = Buffer.create 10 in
  let rec parse ic buf len res =
    try
      let c = input_char ic in
      match c with
      | ' ' | '\t' | '\n' -> 
        if (Buffer.length buf) = 0 then
          eol_or_parse ic buf len res c
        else 
          let number = int_of_string (Buffer.contents buf) in
          begin
            Buffer.reset buf;
            let res = number::res in
            (* printf "added %d\n" number; *)
            let len = len + 1 in
            match n with
            | Some n when len == n -> List.rev res
            | _ -> eol_or_parse ic buf len res c
          end
      |'0'..'9' -> 
        begin
          Buffer.add_char buf c;
          parse ic buf len res
        end
      | _ -> printf "[%c]\n" c; failwith "bad number file"
    with End_of_file -> List.rev res
  and
  eol_or_parse ic buf len res c =
  let eol = c = '\n' in
  if eol then List.rev res 
  else parse ic buf len res
  in
    parse ic buf 0 []

let read_cell_line ?skip ?n file =
  let ic = open_in file in
  let cells = read_cells ~skip ~n ic in
  close_in ic;
  cells

(* NB read_many without ?max could be extended with ?max 
  differently, e.g. 
  match cells,max with
  | [],_ -> acc
  | _,Some n -> ...
  *)
  
let read_many max file =
  let ic = open_in file in
  let rec go len acc = 
    match max with
    | Some n when len = n -> acc
    | _ ->
      let cells = read_cells ic in
      if cells = [] then acc
      else go (len+1) (cells::acc)
    in
  let many = go 0 [] in
  close_in ic;
  List.rev many


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
    | Some n when len = n -> close_in ic; List.rev acc
    | _ ->
      try
        let line = input_line ic in
        go ic (len+1 )(line::acc)
      with End_of_file -> close_in ic; List.rev acc
  in
  go ic 0 []


(* NB: could use List.sort to avoid Array<=>List conversions *)
let get_dirs root =
  let numbers = Str.regexp "^[0-9]+$" in
  let subdirs = Array.to_list (Sys.readdir root) in
  let subdirs = List.filter (fun x -> Str.string_match numbers x 0 && x <> "0") subdirs in
  let subdirs = Array.of_list (List.map int_of_string subdirs) in
  Array.sort compare subdirs;
  let subdirs = List.map string_of_int (Array.to_list subdirs) in
  (* for testing on a short subdir list uncomment below: *)
  (* let subdirs = ["100";"101"] in *)
  subdirs

let dir_data dir date =
  let filename = Filename.basename dir in
  let filename = filename ^ (match date with Some date -> "-"^date | None -> "") in
  let pathname = Filename.concat dir filename in
  pathname

(* general directories walk -- no shared result data *)
let gendirwalk (f:?mincount:int -> ?date:string -> string -> unit) ?date root =
  let subdirs = get_dirs root in  
  match date with
    | Some date ->
      List.iter (fun x -> f (Filename.concat root x) ~date) subdirs
    | None ->
      List.iter (fun x -> f (Filename.concat root x)) subdirs


(* suffix tree walk -- shared result built in st *)
(* here, f takes an option parameter, instead of the optional one above;
   unification is desirable once the best practices are determined *)
  
let st_dirwalk f st ?date root =
  let subdirs = get_dirs root in  
  List.iter (fun x -> f st date (Filename.concat root x)) subdirs

(* sort person-sample files in the numeric order of pXX numbers *)
let sample_person_regexp = Pcre.regexp "(\\d+)$"

let sample_person s =
  int_of_string (Pcre.extract ~rex:sample_person_regexp s).(1)

let compare_sample_persons a b =
  compare (sample_person a) (sample_person b)

(* NB if we'd have an Array.filter, might avoid 
   back and forth array<->list conversions;
   or could use List.sort *)
let get_samples dir =
  let pattern = Str.regexp "^sample-list" in
  let samples = Array.to_list (Sys.readdir dir) in
  let samples = List.filter (fun x -> Str.string_match pattern x 0) samples in
  let samples = Array.of_list samples in
  Array.sort compare_sample_persons samples;
  let samples = Array.to_list samples in
  List.map (fun s -> Filename.concat dir s) samples
