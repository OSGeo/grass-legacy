#include "gis.h"
#include "infx.h"


char* buildInfxQry(ktab,ycol, xcol, dist, pts)
	struct Option *ktab, *ycol, *xcol, *dist; 
	struct Sql *pts;
{
	static char SQL_stmt[1024] ;
	int stat;


           sprintf(SQL_stmt, 
          "SELECT * from %s where 
            point(%s,%s) ",ktab->answer,
            xcol->answer,ycol->answer);
	/* use Postgres graphic operators 
	     @ operator test point in box 
	  cfa 11/98   */
	     
	return (SQL_stmt) ;

}


