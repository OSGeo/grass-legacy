{ 
  test select file for d.what.v.inf -s 
  database=tiger 
  GISdb=spearfish 

  curly brackets are Informix comment delimiters 
}


SELECT tlid,fename,cfcc 
	FROM type1
	WHERE tlid = ? 
