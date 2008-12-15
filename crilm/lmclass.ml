open Baseclient
open Printf

class lmclient (port: string) (order: int) =
object (self)
inherit baseclient

  val mutable handle = Lmclient.null ()
  val mutable handle_i = 0
  val mutable compute_lock = false
  (* NB: creating just for syntax, really done in initializer;
    can't just say sem : Semaphore.t -- what else? *)
  
  method destroy =
    Lmclient.destroy handle
  initializer 
    handle <- Lmclient.create port order;
    handle_i <- Lmclient.int_of_handle handle;
    (* sem <- Semaphore.create 2 *)
    (* Printf.printf "OCaml initializer stores new handle: %d\n" handle_i *)
    (* per Mauricio Fernandez's advice on caml-list *)
    (* Gc.finalise ignore (Lmclient.destroy handle) *)
    (* Gc.finalise ignore self#destroy *)
  method get_port   = port
  method get_order  = order
  method get_handle = handle
  method int_handle = handle_i
  method compute string =
    (* Semaphore.lock sem; *)
    printf "\n&&& ---> (%s) start ---> %s\n" port string;
    let result = Lmclient.compute handle string in
    (* Semaphore.unlock sem; *)
    printf "\n&&& <--- (%s) STOPS <--- %s\n" port string;
    result
  method complete_sentence maxwords li =
    let context = Array.of_list li in
    let result  = Lmclient.complete_sentence handle maxwords context in
    Array.to_list result
end
