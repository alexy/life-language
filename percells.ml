open Printf

let fetch () =
  let dbh = PGOCaml.connect ~database:"sandy" () in
  let column_names = PGSQL(dbh) "select text(attname) from pg_attribute where attrelid = 'percells'::regclass and attnum > 0 and not attisdropped" in
  let rows = PGSQL(dbh) "SELECT * FROM percells ORDER BY person_oid" in
  (* PGOCaml.close dbh *)
  column_names,rows
  
