#include "gis.h"
#include "Vect.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dbvect.h"
#include "glocale.h"

int buildPg(where, map, mapset, color, fillcolor)
     char *where, *map, *mapset;
     int color, fillcolor;
{
    int stat;
    char SQL_stmt[QRY_LENGTH];
    char wherecl[8];

    stat = 1;
    memset(wherecl, '\0', sizeof(wherecl));
    memset(SQL_stmt, '\0', sizeof(SQL_stmt));

    if ((!total_import) && ((!key_string) || (!table_string))) {
    fprintf(stderr, 
    _("You have chosen to relate upon the existing DB table but have not specified TABLE and KEY. -- Exiting...\n"));
    exit(-1);
    }
    if (where) {
	strcpy(wherecl, " where ");

	snprintf(SQL_stmt, QRY_LENGTH,
		 "SELECT Distinct %s from %s %s %s and %s is not null",
		 key_string, table_string, wherecl, where, key_string);
    }
    else
	snprintf(SQL_stmt, QRY_LENGTH,
		 "SELECT Distinct %s from %s where %s is not null",
		 key_string, table_string, key_string);


    stat = runPg(SQL_stmt, map, mapset, color, fillcolor);

    return (stat);
}
