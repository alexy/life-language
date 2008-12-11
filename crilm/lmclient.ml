(* OCaml bindings for SRILM
   Copyright (c) Alexy Khrabrov <deliverable@gmail.com>, 2008.
   Released under GPLv3 -- see FSF.org for details *)

type handle

(* NB can we make null as just a value, not unit-taking function? *)
external  null    : unit -> handle             = "lmclient_null"
external  is_null : handle -> bool             = "lmclient_is_null"
external  int_of_handle : handle -> int        = "lmclient_int_of_handle"

external  create  : string -> int -> handle    = "lmclient_create"
external  destroy : handle -> int              = "lmclient_destroy"
external  compute : handle -> string -> string = "lmclient_compute"
