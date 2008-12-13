(* parallel combinators from the prelude.ml
  http://github.com/kig/preludeml/
 *)

let filter = List.filter
let length = List.length
let rev    = List.rev
let iter   = List.iter
let rev_map = List.rev_map
let foldl   = List.fold_left
let head    = List.hd
let tail    = List.tl
let concat = List.concat
let map f l = rev (rev_map f l) (* NB! *)
let foldr f s l = List.fold_right f l s

let len    = length
let first = head

let int = int_of_float
let chr = char_of_int
let ord = int_of_char

let some x = Some x
(* let none x = None *)
let null = function [] -> true | _ -> false

let (@@) f x = f x
let (|>) x f = f x
let (@.) f g x = f (g x)
let uncurry f (a, b) = f a b
let tuple a b = (a,b)
let fupler f a = (a, f a)

let opt_or o y = match o with Some x -> x | None -> y
let (|?) = opt_or
let optIf p f v = if p v then Some (f v) else None
let maybeE v f o = try f o with _ -> v
let optE f o = maybeE None (some @. f) o

let lessOrEqualTo x y = (<=) y x
let greaterOrEqualTo x y = (>=) y x
let succ x = x + 1
let pred x = x - 1



let pop l =
  let rec aux l res =
    match l with
      | [] -> raise Not_found
      | (h::[]) -> (rev res, h)
      | (h::t) -> aux t (h :: res) in
  aux l []

let foldl1 f l = foldl f (head l) (tail l)
let foldr1 f l = let l,i = pop l in foldr f i l


let unfoldrOpt f init =
  let rec aux f v l =
    match f v with
      | None -> l
      | Some (a, b) -> aux f b (a::l) in
  aux f init []
(**T
  unfoldlOpt (fun x -> if x > 3 then None else Some (x, x+1)) 1 = [1; 2; 3]
  unfoldlOpt (fun i -> if i > 67 then None else Some (char i, i+1)) 65 = ['A';'B';'C']
**)
let unfoldr p f init = unfoldrOpt (optIf p f) init
(**T
  unfoldr (lessThan 4) (fupler succ) 1 = [3; 2; 1]
  unfoldr (lessThan 68) (fuple char succ) 65 = ['C'; 'B'; 'A']
**)
let generateR p f init = unfoldr p (fupler f) init
let range s e =
  if s <= e
  then generateR (greaterOrEqualTo s) pred e
  else generateR (lessOrEqualTo s) succ e
let (--) = range
let generateN f n =
  let rec aux f n res =
    if n < 0 then res
    else aux f (n-1) ((f n) :: res) in
  aux f (n-1) []
let init = generateN

let rx = Pcre.regexp
let smatch pat = Pcre.pmatch ~pat
let rexmatch rex = Pcre.pmatch ~rex
let xmatch s = rexmatch (rx s)

let split ?n sep s = Pcre.split ?max:n ~pat:sep s
let lines s = split "\n" s
let finally finaliser f x =
  let r = try f x with e ->
    ( try finaliser x with _ -> () );
    raise e in
  finaliser x;
  r
let withFile filename f = finally close_in f (open_in_bin filename)
let readAll ch =
  let rec aux ch ret buf =
    match input ch buf 0 4096 with
      | 0 -> Buffer.contents ret
      | b -> Buffer.add_substring ret buf 0 b;
             aux ch ret buf in
  let ret = Buffer.create 4096 in
  let buf = String.create 4096 in
  aux ch ret buf
let readFile filename = withFile filename readAll
let readLines = lines @. readFile

let unfoldr p f init = unfoldrOpt (optIf p f) init
let unfoldl p f init = rev (unfoldr p f init)
let unfoldrUntil p f init = unfoldr (not @. p) f init
let unfoldlUntil p f init = unfoldl (not @. p) f init

let take n lst =
  let rec aux c res l = match c, l with
      | x, (h::t) when x > 0 -> aux (c-1) (h::res) t
      | _ -> rev res in
  aux n [] lst
let rec drop n lst = match n, lst with
  | x, (h::t) when x > 0 -> drop (n-1) t
  | _ -> lst

let splitAt n xs = (take n xs, drop n xs)

let groupsOf n l = if n <= 1 then [l]
  else unfoldlUntil null (splitAt n) l

let splitInto n l = if n <= 1 then [l]
  else groupsOf (int (ceil (float (len l) /. float n))) l

let zipWith f a b =
  let rec aux f a b l = match a,b with
      | (x::xs), (y::ys) -> aux f xs ys ((f x y)::l)
      | _ -> l in
  rev @@ aux f a b []
let zip a b = zipWith tuple a b

let slen = String.length
let join = String.concat
let sinit f l =
  let s = String.create l in
  for i=0 to l-1 do String.unsafe_set s i (f i) done;
  s
let binit f l =
  let s = String.create l in
  for i=0 to l-1 do String.unsafe_set s i (chr (f i)) done;
  s
  
let ainit f l = Array.init l f
let alen = Array.length

let coreCount () =
  let countCores l = filter (xmatch "^processor\\s*:") l |> length in
  optE (countCores @. readLines) "/proc/cpuinfo"

(* set the default properly for your system *)
let global_process_count = ref (coreCount () |? 2)

let invoke f x =
  flush stdout;
  let input, output = Unix.pipe() in
  match Unix.fork() with
  | -1 -> let v = f x in fun () -> v
  | 0 ->
      global_process_count := 1; (* no further implicit parallelization *)
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
      Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
      close_out output;
      exit 0
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v = Marshal.from_channel input in
        ignore (Unix.waitpid [] pid);
        close_in input;
        match v with `Res x -> x | `Exn e -> raise e

(*
  The "par_"-functions are intended as plumbing to build other functions
  on top of. As such, their semantics are quite broken for common usage
  and you should use the "p*"-functions instead.
*)

(* lockstep iteration, unshifting new job after popped job completes
   a polling system would fare better with uneven job runtimes
*)
let par_iter ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let rec aux f n procs l =
    let n,procs = if n >= process_count
    then match procs with [] -> 0,[] | lst ->
      let l, last = pop lst in
      (last (); ((n-1), l))
    else n,procs in
    match l with
      | [] -> iter (fun f -> f ()) procs
      | (h::t) -> aux f (n+1) ((invoke f h) :: procs) t in
  aux f 0 [] l

(* lockstep iteration, unshifting new job after popped job completes
   a polling system would fare better with uneven job runtimes
*)
let par_map ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let rec aux f n procs res l =
    let n,res,procs = if n >= process_count
    then match procs with
        | [] -> 0,res,[]
        | lst -> let l,last = pop lst in
                 ((n-1), (last ())::res, l)
    else n,res,procs in
    match l with
      | [] -> (rev res) @ (rev_map (fun f -> f ()) procs)
      | (h::t) -> aux f (n+1) ((invoke f h) :: procs) res t in
  aux f 0 [] [] l

(*
  Splits n into process_count continuous segments,
  executes each in its own process.
*)
let pforN ?process_count f n =
  let process_count = process_count |? !global_process_count in
  let plen = int (ceil (float n /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (n - start) in
    for j = start to start+len-1 do
      f j
    done in
  par_iter ~process_count process (0--(process_count-1))

let mapReduce partition distribute process combine input =
  partition input |> distribute process |> combine

let par_mapReduce ?process_count ~combine ~process l =
  let process_count = process_count |? !global_process_count in
  splitInto process_count l |> par_map ~process_count process |> combine

let pmapReduce combine process = par_mapReduce ~combine ~process

let pfoldl r f init = pmapReduce (foldl1 r) (foldl f init)
let pfoldl1 f = pmapReduce (foldl1 f) (foldl1 f)
let pfoldr r f init = pmapReduce (foldr1 r) (foldr f init)
let pfoldr1 f = pmapReduce (foldr1 f) (foldr1 f)

let piter f = pmapReduce ignore (iter f)
let pmap f = pmapReduce concat (map f)
let pfilter f = pmapReduce concat (filter f)

let pfoldlSeqN ?process_count n r f init l =
  foldl (fun acc il -> r acc (pfoldl ?process_count r f init il))
        init (groupsOf n l)

let pfoldl1SeqN ?process_count n f l =
  pfoldlSeqN ?process_count n f f (first l) (tail l)

let piterSeqN ?process_count n r f l =
  iter (fun l -> iter r (pmap ?process_count f l)) (groupsOf n l)


let pinit ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let plen = int (ceil (float l /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (l - start) in
    init (fun j -> f (start + j)) len in
  concat (par_map ~process_count process (0--(process_count-1)))

let pzipWith ?process_count f a b =
  let process_count = process_count |? !global_process_count in
  let len = min (len a) (len b) in
  let plen = int (ceil (float len /. float process_count)) in
  let aspl = groupsOf plen a in
  let bspl = groupsOf plen b in
  concat (par_map ~process_count (uncurry (zipWith f)) (zip aspl bspl))


(* let par_bainit ?process_count ?layout kind f l =
  let ba = bacreateShared ?layout kind l in
  pforN ?process_count (fun i -> Bigarray.Array1.set ba i (f i)) l;
  ba
let pbainit = par_bainit
 *)

let par_sinit ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let plen = int (ceil (float l /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (l - start) in
    sinit (fun j -> f (start + j)) len in
  join "" (par_map ~process_count process (0--(process_count-1)))
let psinit = par_sinit

let pbinit ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let plen = int (ceil (float l /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (l - start) in
    binit (fun j -> f (start + j)) len in
  join "" (par_map ~process_count process (0--(process_count-1)))


let par_ainit ?process_count f l =
  let process_count = process_count |? !global_process_count in
  let plen = int (ceil (float l /. float process_count)) in
  let process i =
    let start = plen * i in
    let len = min plen (l - start) in
    ainit (fun j -> f (start + j)) len in
  Array.concat (par_map ~process_count process (0--(process_count-1)))
let painit = par_ainit


let paZipWith ?process_count f a b =
  let process_count = process_count |? !global_process_count in
  let len = min (alen a) (alen b) in
  painit ~process_count (fun i ->
    f (Array.unsafe_get a i) (Array.unsafe_get b i)
  ) len

let psZipWith ?process_count f a b =
  let process_count = process_count |? !global_process_count in
  let len = min (slen a) (slen b) in
  psinit ~process_count (fun i ->
    f (String.unsafe_get a i) (String.unsafe_get b i)
  ) len

let pbZipWith ?process_count f a b =
  let process_count = process_count |? !global_process_count in
  let len = min (slen a) (slen b) in
  pbinit ~process_count (fun i ->
    f (ord (String.unsafe_get a i)) (ord (String.unsafe_get b i))
  ) len
