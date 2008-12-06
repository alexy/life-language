open Printf

let () =
  let dbh = PGOCaml.connect () in

  let cells person =
    PGSQL(dbh) "SELECT celltower_oid FROM cellspan WHERE person_oid = $person"
  in

  let results = cells 1 in
  List.iter (
    fun (cell') ->
      let cell = match cell' with Some c -> c | None -> 0 in
      printf "%ld\n" cell
  ) results;

  PGOCaml.close dbh
