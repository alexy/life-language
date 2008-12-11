class lmclient (port: string) (order: int) =
object (self)
  (* val mutable port   = port
  val mutable order  = order *)
  val mutable handle = Lmclient.null ()
  val mutable handle_i = 0
  method get_handle = handle
  method int_handle = handle_i
  method compute string =
    Lmclient.compute handle string
  method destroy =
    Lmclient.destroy handle
  initializer 
    handle <- Lmclient.create port order;
    handle_i <- Lmclient.int_of_handle handle;
    Printf.printf "OCaml initializer stores new handle: %d\n" handle_i
end
