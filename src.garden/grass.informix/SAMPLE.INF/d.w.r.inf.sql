{ 
  test select file for d.what.r.inf -s 
  database=tiger 
  GISdb=spearfish 

  curly brackets are Informix comment delimiters 
}


SELECT tractbna, stusab, statece, funcstat, pop100 
	FROM stf1_main
	WHERE tractbna = ? 
