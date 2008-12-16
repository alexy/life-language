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
