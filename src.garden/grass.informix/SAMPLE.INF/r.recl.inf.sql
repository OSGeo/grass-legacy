{ 
  test select file for r.reclass.inf  
  database=tiger, 
  GISdb=spearfish 

  curly brackets are Informix comment delimiters 
}

select tractbna,p0050002 from stf1_person
	where stf1_person.p0050002 <= 2000 ;

select tractbna,p0050002 from stf1_person
	where stf1_person.p0050002 > 2000 and
	stf1_person.p0050002 <= 3000 ;

select tractbna,p0050002 from stf1_person
	where stf1_person.p0050002 > 3000 

