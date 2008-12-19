open Printf

(* initially I thought I'd need to revert a buffer
  to parse a string into a number; in fact not,
  but why waste a good fun -- posted to codepad.org as well *)
(* http://codepad.org/I5sPOxrA *)
let revert_buffer ibuf =
  let len = (Buffer.length ibuf) in
  let obuf = Buffer.create len in
  let rec drop n =
    match n with
    | -1 -> Buffer.contents obuf
    | _  -> begin
      Buffer.add_char obuf (Buffer.nth ibuf n);
      drop (n-1)
    end in
  drop (len-1)


(* terminate on 1 is equivalent to calling with n-1 *)
let rec skip_n_spaces ic n =
  let c = input_char ic in
  match c with 
  | ' ' | '\t' | '\n' -> if n == 1 then () else skip_n_spaces ic (n-1)
  | _ -> skip_n_spaces ic n
      

(* NB: add EOF handling *)
let skip_n_words ic n =
  let in_word = ref false in
  let rec go ic n =
    let c = input_char ic in
    match c with
    | ' ' | '\t' | '\n' ->  
      if !in_word then 
        begin
          if n == 0 then () 
          else begin 
            in_word := false; 
            go ic (n-1) 
          end
        end
      else (* multiple white spaces *)
        go ic n
    | _ -> 
      begin 
        in_word := true; 
        go ic n
      end
    in
    if n <= 0 then () 
    else go ic (n-1)
let read_cells ?skip ?n filename =
  let ic = open_in filename in begin
  match skip with
    | Some n -> skip_n_words ic n
    | None -> () 
  end;
  let buf = Buffer.create 10 in
  let rec parse ic buf len res =
    try
      let c = input_char ic in
      match c with
      | ' ' | '\t' -> if (Buffer.length buf) == 0 
        then parse ic buf len res 
        else let number = int_of_string (Buffer.contents buf) in
        begin
          Buffer.reset buf;
          let res = number::res in
          let len = len + 1 in
          match n with
          | Some n when len == n -> close_in ic; List.rev res
          | _ -> parse ic buf len res
        end
      |'0'..'9' -> 
        begin
          Buffer.add_char buf c;
          parse ic buf len res
        end
      | '\n' -> close_in ic; List.rev res
      | _ -> printf "[%c]\n" c; failwith "bad number file"
    with End_of_file -> close_in ic; List.rev res
    in
    parse ic buf 0 []


(* -- using DynArray, use extlib or batteries for that
let read_cells2 ?skip ?n filename =
  let ic = open_in filename in begin
  match skip with
    | Some n -> skip_n_words ic n
    | None -> () 
  end;
  let buf = Buffer.create 10 in
  let rec parse ic buf len res =
    try
      let c = input_char ic in
      match c with
      | ' ' | '\n' -> if (Buffer.length buf) == 0 
        then parse ic buf len res 
        else let number = int_of_string (Buffer.contents buf) in
        begin
          Buffer.reset buf;
          DynArray.add res number;
          let len = len + 1 in
          match n with
          | Some n when len == n -> close_in ic; (DynArray.to_array res)
          | _ -> parse ic buf len res
        end
      |'0'..'9' -> 
        begin
          Buffer.add_char buf c;
          parse ic buf len res
        end
      | _ -> printf "[%c]\n" c; failwith "bad number file"
    with End_of_file -> close_in ic; (DynArray.to_array res)
    in
    parse ic buf 0 (DynArray.create ())
 *)

(* NB: add EOF handling *)
let rec skip_lines ic n =
  match n with
  | 0 -> ()
  | _ when n < 0 -> failwith "can only skip nonnegative number of lines"
  | _ -> ignore (input_line ic); skip_lines ic (n-1)
  

let read_lines ?skip ?n filename =
  let ic = open_in filename in
  begin
    match skip with
      | Some n -> skip_lines ic n
      | None -> ()
  end;
  let rec go ic len acc =
    match n with
    | Some n when len == n -> close_in ic; List.rev acc
    | _ ->
      try
        let line = input_line ic in
        go ic (len+1 )(line::acc)
      with End_of_file -> close_in ic; List.rev acc
  in
  go ic 0 []
