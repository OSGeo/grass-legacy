#include "gis.h"
#include "pg.h"
#include <stdio.h>


char *buildPg(ktab, keycat, curcat)
     char *ktab, *keycat;
     int curcat;
{


    static char SQL_stmt[QRY_LENGTH];


    snprintf(SQL_stmt, QRY_LENGTH,
	     "SELECT * from %s where %s=%d", ktab, keycat, curcat);

    return (SQL_stmt);

}

char *buildPgSite(ktab, ycol, xcol)
     char *ktab, *ycol, *xcol;
{
    static char SQL_stmt[QRY_LENGTH];

    snprintf(SQL_stmt, QRY_LENGTH,
	     "SELECT * from %s where point(%s,%s) ", ktab, xcol, ycol);

    return (SQL_stmt);

}
