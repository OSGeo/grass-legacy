#include "gis.h"
#include "infx.h"


char* buildInfxQry(ktab,ycol, xcol, dist, pts, joinargs)
	struct Option *ktab, *ycol, *xcol, *dist; 
	struct Sql *pts;
	char *joinargs[];
{
	static char SQL_stmt[1024] ;
	int stat;


/* Postquel query by J.Soimasuo
	  fprintf(fp,"SELECT unique (%s.all) \n",
		ktab->answer);
	  fprintf(fp,"where ( ( (%s.%s - %f) * (%s.%s - %f) +\n",
		ktab->answer,xcol->answer, pts->centX, 
		ktab->answer,xcol->answer, pts->centX);
	  fprintf(fp,"(%s.%s - %f) * (%s.%s - %f) ) < %f )\n",
		ktab->answer,ycol->answer, pts->centX,
		ktab->answer,ycol->answer,pts->centY, pts->rad2 );  
	}
 */
 
           sprintf(SQL_stmt, 
          "SELECT * from %s where 
            point(%s,%s) ",ktab->answer,
            xcol->answer,ycol->answer);
	/* use Postgres graphic operators 
	     @ operator test point in box 
	  cfa 11/98   */
	     
	return (SQL_stmt) ;

}


