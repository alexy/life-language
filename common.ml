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
