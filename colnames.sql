 SELECT
     a.attname as "Column",
     pg_catalog.format_type(a.atttypid, a.atttypmod) as "Datatype"
 FROM
     pg_catalog.pg_attribute a
 WHERE
     a.attnum > 0
     AND NOT a.attisdropped
     AND a.attrelid = (
         SELECT c.oid
         FROM pg_catalog.pg_class c
             LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
         WHERE c.relname ~ '^(percells)$'
             AND pg_catalog.pg_table_is_visible(c.oid)
     )
 ;
 
 -- or
 
 select a.attname from pg_attribute a, pg_class b 
 where b.relfilenode=a.attrelid and b.relname='percells' 
 and a.attname not in ('tableoid','cmax','xmax','cmin','xmin','ctid')
 
 -- or
 
 select attname from pg_attribute
 where attrelid = 'percells'::regclass -- 'my_schema.my_tablename'::regclass
       and attnum > 0 and not attisdropped;
 