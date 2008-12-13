open Printf

IFDEF USE_POSTGRES THEN 
let percells_dataframe () =
  printf "fetching dataframe from database\n";
  let colnames,percells = Percells.fetch () in
  let colnames = List.map (function | Some x -> x | None -> "") colnames in
  let matrix = matrix_of_percells percells in
  colnames,matrix
END
  
let save dataframe filename =
  let ob = open_out_bin filename in
  Marshal.to_channel ob dataframe [];
  close_out ob
  
let load filename =
  let ib = open_in_bin filename in
  printf "loading dataframe from file %s\n" filename;
  let ((colnames: string list), (matrix: int array array)) = Marshal.from_channel ib in
  close_in ib;
  colnames,matrix
  
let get ?fromfile () =
  match fromfile with
  | Some file -> load file
  | None      -> IFDEF USE_POSTGRES THEN percells_dataframe () ELSE failwith "no fatabase for you" END
