#include "gis.h"
#include "infx.h"


char* buildInfxQry(ktab,ycol, xcol, dist)
	struct Option *ktab, *ycol, *xcol, *dist;
{
	static char SQL_stmt[1024] ;


           sprintf(SQL_stmt, 
          "SELECT * from %s where 
            point(%s,%s) ",ktab->answer,
            xcol->answer,ycol->answer);
	/* use Postgres graphic operators 
	     @ operator test point in box 
	  cfa 11/98   */
	     
	return (SQL_stmt) ;

}


