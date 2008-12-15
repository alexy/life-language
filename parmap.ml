let rev     = List.rev
let rev_map = List.rev_map
let map f l = rev (rev_map f l)

let (|>) x f = f x
let opt_or o y = match o with Some x -> x | None -> y
(**T
  opt_or None 0 = 0
  opt_or (Some 10) 0 = 10
**)
let (|?) = opt_or

let pop l =
  let rec aux l res =
    match l with
      | [] -> raise Not_found
      | (h::[]) -> (rev res, h)
      | (h::t) -> aux t (h :: res) in
  aux l []

(* let coreCount () =
  let countCores l = filter (xmatch "^processor\\s*:") l |> length in
  optE (countCores @. readLines) "/proc/cpuinfo" *)

let global_process_count = ref ((* coreCount () |? *) 2)


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
