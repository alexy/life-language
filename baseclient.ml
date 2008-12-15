class virtual baseclient =
  object
    val mutable sem = Semaphore.create 1
    method virtual compute : string -> string
    method virtual destroy : int
    method lock   = Semaphore.lock   sem;
    method unlock = Semaphore.unlock sem;
  end
