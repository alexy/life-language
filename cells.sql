create table cellspan_periods as 
  select person_oid,count(*) as "total" from cellspan 
  group by person_oid;

-- repeat this for all month ranges, now 2004-07-01..2005-05-01
update cellspan_periods as t set "2004-11-01" = c from 
  (select person_oid,count(*) as c from cellspan 
    where starttime < '2004-11-01' group by person_oid) as i 
  where t.person_oid = i.person_oid;
  
