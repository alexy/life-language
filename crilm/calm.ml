open Printf

let () =
  print_endline "let's go";

  let portbase = 10000 in
  let lms = List.map (fun i ->
    let port = portbase + i in
    let lm = Lmclient.create (sprintf "%d@localhost" port) 5 in
    printf "handle = %d\n" lm; flush stdout;
    let results = Lmclient.compute 1 "/Users/alexyk/cells/seq40" in
    Printf.printf "=> %s\n" results;
    lm) [1;3]
  in
  print_endline (String.concat "," (List.map string_of_int lms));
  List.iter (fun lm ->
    let num = Lmclient.destroy lm in
    printf "num = %d\n" num) (List.rev lms)