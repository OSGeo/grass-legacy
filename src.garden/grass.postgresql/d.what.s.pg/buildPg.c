#include "what.h"


char *buildPg(ktab, ycol, xcol)
     struct Option *ktab, *ycol, *xcol;
{
    static char SQL_stmt[QRY_LENGTH];


    sprintf(SQL_stmt,
	    "SELECT * from %s where point(%s,%s)", ktab->answer,
	    xcol->answer, ycol->answer);
    /* use Postgres graphic operators 
       @ operator test point in box 
       cfa 11/98   */

    return (SQL_stmt);

}
