/* d.vect.pg  */
/* modifications 11/1998 Carl Anderson */

#include "gis.h"
#include "Vect.h"
#include <stdio.h>
#include <string.h>
#include "dbvect.h"

int buildPg(key, where, table, map, mapset, color, fillcolor)
     char *key, *where, *table, *map, *mapset;
     int color, fillcolor;
{
    int stat = 1;
    char SQL_stmt[QRY_LENGTH];
    char wherecl[8];

    memset(SQL_stmt, '\0', sizeof(SQL_stmt));
    memset(wherecl, '\0', sizeof(wherecl));

    if (where) {
	strcpy(wherecl, " where ");

	snprintf(SQL_stmt, QRY_LENGTH,
		 "SELECT Distinct %s from %s %s %s and %s is not null", key,
		 table, wherecl, where, key);
    }
    else
	snprintf(SQL_stmt, QRY_LENGTH,
		 "SELECT Distinct %s from %s where %s is not null", key,
		 table, key);


    stat = runPg(SQL_stmt, map, mapset, color, fillcolor);

    return (stat);
}
