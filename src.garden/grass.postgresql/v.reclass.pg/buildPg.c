#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "dbvect.h"

int buildPg(key, col, table, where, input, output, vtype, disolve)
     char *key;
     char *col;
     char *table;
     char *where;
     char *input;
     char *output;
     char *vtype;
     int disolve;
{
    char SQL_stmt[QRY_LENGTH];
    char wherecl[QRY_LENGTH];

    int i;

    memset(SQL_stmt, '\0', sizeof(SQL_stmt));
    memset(wherecl, '\0', sizeof(wherecl));

    if (where)
	snprintf(wherecl, QRY_LENGTH, "%s and", where);
    snprintf(SQL_stmt, QRY_LENGTH,
	     "SELECT %s,%s from %s where %s %s is not null and %s is not null order by %s,%s",
	     key, col, table, wherecl, key, col, key, col);

    i = runPg(SQL_stmt, input, output, vtype, disolve);
    return (i);
}
