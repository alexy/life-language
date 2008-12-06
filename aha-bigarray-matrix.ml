let m = Array.make_matrix 3 3 0l
m.(0).(0) <- 1l
m.(0).(2) <- 5l
m.(1).(0) <- 7l
m.(2).(1) <- 8l
m.(2).(2) <- 9l

(* 
# m;;
- : int32 array array = [|[|1l; 0l; 5l|]; [|7l; 0l; 0l|]; [|0l; 8l; 9l|]|]
   *)

module A2 = Bigarray.Array2

let m2 = A2.of_array Bigarray.int32 Bigarray.fortran_layout m

let m1 = A2.slice_right m2 3


module A1 = Bigarray.Array1

List.map (fun i -> A1.get m1 i) [1;2;3]

(* 
- : int32 list = [5l; 0l; 9l]  
   *)