open Printf

(* person and upto are option types, resp. Option Int32 and Option string *)
let dbcells dbh person upto =
  match upto with
    | Some date's ->
      let ymd's = Str.split_delim (Str.regexp "-") date's in
      let ymd = List.map int_of_string ymd's in
      begin
        match ymd with
          | [year;month;day] -> let date = CalendarLib.Calendar.make year month day 0 0 0 in
            begin
              match person with
                | Some person -> PGSQL(dbh) "SELECT celltower_oid FROM cellspan \
                    WHERE person_oid = $person AND starttime < $date ORDER BY starttime"
                | None        -> PGSQL(dbh) "SELECT celltower_oid FROM cellspan \
                    WHERE starttime < $date ORDER BY starttime "
            end
          | _ -> failwith "bad date"
      end
    | None ->
      begin
        match person with
          | Some person -> PGSQL(dbh) "SELECT celltower_oid FROM cellspan \
              WHERE person_oid = $person ORDER BY starttime"
          | None        -> PGSQL(dbh) "SELECT celltower_oid FROM cellspan \
              ORDER BY starttime"
      end

(* cells for a single person -- per person cells *)
let percells dbh person upto =  
 
  let person's = Int32.to_string person in

  let home = Unix.getenv "HOME" in
  let root = Filename.concat home "cells" in
  let perms = 0o755 in
  
  let dir = Filename.concat root person's in
  printf "using or creating directory %s\n" dir;
  begin
  try
    Unix.mkdir dir perms
  with Unix.Unix_error _ -> () end; 
  
  let date_suffix =
    match upto with
      | Some date's -> "-"^date's
      | None -> ""
    in
  let outname = Filename.concat dir (person's ^ date_suffix) in
  printf "writing file %s\n" outname;  
  let out = open_out outname in
  let results = dbcells dbh (Some person) upto in

  List.iter (
    fun cell ->
      output_string out (Int32.to_string cell);
      output_string out " "
  ) results;

  close_out out;
  printf "finished %s\n" person's
    

let () =
  let upto = 
    match (Array.length Sys.argv) with
      | 2 -> Some Sys.argv.(1)
      | 1 -> None
      | _ -> failwith "provide a cutoff date in the format 2004-11-01 or none for all" in
  
  let dbh = PGOCaml.connect ~database:"sandy" () in

  let persons = PGSQL(dbh) "SELECT DISTINCT person_oid FROM cellspan" in
  (* let persons = [40_l] in *)
  
  List.iter(
    fun x -> percells dbh x upto
    ) persons;
    
  PGOCaml.close dbh;
  printf "total %d people done\n" (List.length persons)