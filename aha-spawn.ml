(* with simple fork *)
let spawn command_line = if Unix.fork () = 0 then ignore (Unix.system command_line)

spawn "ngram -lm /Users/alexyk/cells/1/mitr-wb5-1-2004-11-01.lm -order 5 -server-port 10002";;

(* with fork *)

let command_line = "ngram -lm /Users/alexyk/cells/1/mitr-wb5-1-2004-11-01.lm -order 5 -server-port" in
let port = 10011 in
let port's = string_of_int port in
let args = Array.of_list (Str.split (Str.regexp " ") command_line) in
let args = Array.append args [|port's|] in
match Unix.fork () with 
| 0 -> Unix.execvp args.(0) args 
| x -> printf "child: %d\n" x


(* with create_process *)

let command_line = "ngram -lm /Users/alexyk/cells/1/mitr-wb5-1-2004-11-01.lm -order 5 -server-port" in
let port = 10012 in
let port's = string_of_int port in
let args = Array.of_list (Str.split (Str.regexp " ") command_line) in
let args = Array.append args [|port's|] in
let devnull = "/dev/null" in
let in_devnull  = open_in  devnull in
let out_devnull = open_out devnull in
let in_fd = Unix.descr_of_in_channel  in_devnull in  (* stdin *)
let out_fd = Unix.descr_of_out_channel out_devnull in (* stdout *)
let err_fd = out_fd in (* Unix.descr_of_out_channel stderr *)
Unix.create_process args.(0) args in_fd out_fd err_fd
