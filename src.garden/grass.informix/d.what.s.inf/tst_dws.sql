{ test select file for d.vect.db -s, 
  DB=seco_wells, GISdb=SECO }
 { curly brackets are Informix comment delimiters }


{ WHERE clause calculates point in circumference }
SELECT * from well where
(  ( ( utm_east-?) * (utm_east-?)  +
     ( utm_north-?) * (utm_north-?) ) < ? )
  AND utm_east > 0 AND utm_north >0
