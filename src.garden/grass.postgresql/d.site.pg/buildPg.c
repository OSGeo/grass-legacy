#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include <stdio.h>
#include "dbsite.h"

int buildPg(table, x, y, cat, where, map, plotargs)
     char *table;
     char *x;
     char *y;
     char *cat;
     char *where;
     char *map;
     char *plotargs[];
{
    char SQL_stmt[QRY_LENGTH];
    char wherecl[QRY_LENGTH];
    char catcl[80];
    int i;

    memset(SQL_stmt, '\0', sizeof(SQL_stmt));
    memset(wherecl, '\0', sizeof(wherecl));
    memset(catcl, '\0', sizeof(catcl));

    if (where) {
	snprintf(wherecl, QRY_LENGTH, "%s and", where);
    }

    if (cat) {
	snprintf(catcl, 80, ", %s ", cat);
    }

    snprintf(SQL_stmt, QRY_LENGTH,
	     "SELECT %s,%s %s from %s where %s %s is not null and %s is not null order by %s,%s",
	     x, y, catcl, table, wherecl, x, y, x, y);

    i = runPg(SQL_stmt, map, plotargs);
    return (i);
}
