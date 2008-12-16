(* can this be done with one Pcre.<exec-type-op>?: *)
let elem_match list regex's =
  let re = Pcre.regexp regex's in
  try
    let elem = List.find (fun x -> Pcre.pmatch ~rex:re x) list in
    let meat = (Pcre.extract ~rex:re elem).(1) in
    Some meat
  with Not_found -> None
