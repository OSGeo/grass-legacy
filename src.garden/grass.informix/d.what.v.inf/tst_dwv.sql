{ test select file for d.vect.inf -s, database=tiger, GISdb=SECO }
 { curly brackets are Informix comment delimiters }

SELECT * FROM feature, cfcc_lookup
WHERE record_num=?
AND feature.cfcc = cfcc_lookup.cfcc
