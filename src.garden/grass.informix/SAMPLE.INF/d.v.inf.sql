{ 
  test select file for d.vect.inf -s 
  database=tiger 
  GISdb=spearfish 

  curly brackets are Informix comment delimiters 
}


SELECT UNIQUE tlid 
	FROM type1
	WHERE cfcc = "A41" 
	ORDER BY tlid 
