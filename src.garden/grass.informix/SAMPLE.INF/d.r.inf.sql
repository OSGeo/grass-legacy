{ test select file for d.rast.db -s, database=tiger, GISdb=spearfish }
{ curly brackets are Informix comment delimiters }

	SELECT UNIQUE tractbna,pop100 FROM
	stf1_main
	WHERE pop100 IS NOT NULL
	ORDER BY pop100 


{ Note: The ORDER BY phrase must be included and
	specify the COL to base the reclass on
}

