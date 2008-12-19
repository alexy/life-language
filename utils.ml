(* can this be done with one Pcre.<exec-type-op>?: *)
let elem_match list regex's =
  let re = Pcre.regexp regex's in
  try
    let elem = List.find (fun x -> Pcre.pmatch ~rex:re x) list in
    let meat = (Pcre.extract ~rex:re elem).(1) in
    Some meat
  with Not_found -> None

let rematch elem regex's =
  let re = Pcre.regexp regex's in
  try
    let meat = (Pcre.extract ~rex:re elem).(1) in
    Some meat
  with Not_found -> None


let join ?(sep=" ") ili =
  let sli = List.map string_of_int ili in
  String.concat sep sli

let rec take n l =
  let rec go n l acc =
  match l with
  | x::xs when n > 0 -> go (n-1) xs (x::acc) 
  | _ -> List.rev acc in
  go n l []
  
let rec drop n l =
  let rec go n l =
  match l with
  | x::xs when n > 0 -> go (n-1) xs 
  | xs -> xs in
  go n l

(* The Fisher-Yates shuffle (aka Knuth Shuffle), by Martin Bishop,
   http://forge.ocamlcore.org/snippet/detail.php?type=snippet&id=1
 *)
let shuffle a =
  for n = ((Array.length a) - 1) downto 0 do
    Random.self_init ();
    let k = Random.int (n + 1) in
    let temp = a.(n) in
      a.(n) <- a.(k);
      a.(k) <- temp
  done