{ test select file for d.vect.db -s, database=tiger, GISdb=SECO }
 { curly brackets are Informix comment delimiters }

{ Test for tiger line segment data, requires 1:1 index in db and GIS for TLID num }

SELECT UNIQUE record_num FROM feature
WHERE feature_name matches "Ranch Rd*"
AND record_num IS NOT NULL


{ Test for tiger line segment data, requires 1:1 index in db and GIS for TLID num }
{
SELECT UNIQUE record_num FROM feature
WHERE feature_name matches "Sabinal River"
AND record_num IS NOT NULL
ORDER BY record_num 
}

{ test for bandera_all_tgr }
{
SELECT UNIQUE record_num FROM feature
WHERE cfcc matches "A2*"
AND record_num IS NOT NULL
}
