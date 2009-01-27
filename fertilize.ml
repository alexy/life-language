let quant length points =
  let res = length / points in
  if res >= 1 then res else 1


let fertility ?(hashsize=1000) points li =
  let len = List.length li in
  let step = quant len points in
  let news = Hashtbl.create hashsize in
  let rec go h li i res =
    let res = if i mod step = 0 || i = len then
      (i, Hashtbl.length h)::res
  else res in
  match li with
  | x::xs -> begin 
                Utils.incr_hash h x;
                go h xs (i+1) res
              end
  | _ -> List.rev res in 
  go news li 0 []


let () =
  let points = int_of_string Sys.argv.(1) in
  let cellfile = Sys.argv.(2) in
  let cells = Seq.read_cell_line cellfile in
  let graph = fertility points cells in
  (* print_endline (Utils.show_pairs graph) *)
  Utils.graph_pairs graph