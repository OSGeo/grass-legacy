{ 
  DB=tiger, 
  GISdb=spearfish 

	WHERE clause calculates point in circumference.
	"?" is used to hold coordinate location passed
	from GRASS
}

SELECT * from type7 where
(  ( ( utm_e-?) * (utm_e-?)  +
     ( utm_n-?) * (utm_n-?) ) < ? )
  AND utm_e > 0 AND utm_n >0

