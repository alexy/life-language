open Baseclient

class syclient (person: int) (port: int) (order: int) =
object (self)
  inherit baseclient
  val mutable command = ""
  initializer 
    let prefix = Printf.sprintf "ngram -use-server %d@localhost -order %d -debug 1 -ppl " port order in
    command <- prefix;
  method destroy     = 0
  method get_person  = person
  method get_port    = port
  method get_order   = order
  method get_command = command
  method compute arg =
    let command_line = command ^ arg in
    Process.read command_line
end
