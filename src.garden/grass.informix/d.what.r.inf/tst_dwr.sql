{ test select file for d.what.r.db -s, 
  DB=seco_soils, GISdb=SECO }
 { curly brackets are Informix comment delimiters }


{ returns attributes from 3 tables as a list }

SELECT * from seco_soil_cats, mapunit, compyld
WHERE grass_cat=? AND
seco_soil_cats.muid=mapunit.muid AND
mapunit.muid=compyld.muid
