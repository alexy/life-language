open Baseclient

class clclient (person: int) (port: int) (order: int) (lm: string) =
object (self)
  inherit baseclient
  val mutable command = ""
  initializer 
    let prefix = Printf.sprintf "ngram -lm %s -order %d -debug 1 -ppl " lm order in
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
