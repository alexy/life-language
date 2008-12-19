class virtual baseclient =
  object
    method virtual compute : string -> string
    method virtual create : unit
    method virtual destroy : int
  end
