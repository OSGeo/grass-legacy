{ test select file for d.rast.db -s, database=seco_soils, GISdb=SECO }
 	{ curly brackets are Informix comment delimiters }

SELECT UNIQUE seco_soil_cats.grass_cat,cropname FROM seco_soil_cats,compyld
WHERE compyld.muid = seco_soil_cats.muid 
AND cropname IS NOT NULL
ORDER BY cropname 


{ Note: The ORDER BY phrase must be included and
	specify the COL to base the reclass on
}


