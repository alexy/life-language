open Baseclient

class lmclient (port: string) (order: int) =
object (self)
inherit baseclient

  val mutable handle = Lmclient.null ()
  val mutable handle_i = 0
  val mutable compute_lock = false
  
  method destroy =
    Lmclient.destroy handle
  initializer 
    handle <- Lmclient.create port order;
    handle_i <- Lmclient.int_of_handle handle;
    (* Printf.printf "OCaml initializer stores new handle: %d\n" handle_i *)
    (* per Mauricio Fernandez's advice on caml-list *)
    (* Gc.finalise ignore (Lmclient.destroy handle) *)
    (* Gc.finalise ignore self#destroy *)
  method get_port   = port
  method get_order  = order
  method get_handle = handle
  method int_handle = handle_i
  method compute string =
    while compute_lock do () done;
    compute_lock <- true;
    (* Printf.printf "client %d starts computing\n" handle_i; *)
    let result = Lmclient.compute handle string in
    compute_lock <- false;
    (* Printf.printf "client %d stops computing\n" handle_i; *)
    result
  method complete_sentence maxwords li =
    let context = Array.of_list li in
    let result  = Lmclient.complete_sentence handle maxwords context in
    Array.to_list result
end
