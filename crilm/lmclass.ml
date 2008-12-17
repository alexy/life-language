open Baseclient
open Printf

class lmclient (port: string) (order: int) (vocab: string) =
object (self)
inherit baseclient

  val mutable handle = Lmclient.null ()
  val mutable handle_i = 0
  val mutable finalize = (0, Lmclient.null ())
  (* val mutable sem = Semaphore.create 1 *)
  
  method destroy =
    Lmclient.destroy handle
  initializer 
    handle   <- Lmclient.create port order vocab;
    finalize <- (0, handle);
    handle_i <- Lmclient.int_of_handle handle;
    (* see caml-list and #ocaml log of 2008-12-12 *)
    (* NB: add a check whether we manually called destroy, then do nothing! *)
    Gc.finalise (fun (_,x) -> ignore (Lmclient.destroy x)) finalize
  method get_port   = port
  method get_order  = order
  method get_handle = handle
  method int_handle = handle_i
  method compute string =
    (* Semaphore.lock sem; *)
    (* Printf.printf "client %d starts computing\n" handle_i; *)
    let result = Lmclient.compute handle string in
    (* Semaphore.unlock sem; *)
    (* Printf.printf "client %d stops computing\n" handle_i; *)
    result
  method complete_sentence maxwords li =      
    let ls = List.map string_of_int li in
    printf "complete sentence, handle %d, port %s, prefix [%s], maxwords %d\n"
      handle_i port (String.concat "," ls) maxwords; flush stdout;
    let context = Array.of_list ls in
    let result  = Lmclient.complete_sentence handle maxwords context in
    let rs = Array.to_list result in
    (* printf "result => %s\n" (String.concat "," rs); flush stdout; *)
    (* rs *)
    let ri = List.map int_of_string rs in ri
end
