#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include <stdio.h>
#include "dbrast.h"

int buildPg(key, col, lab, table, where, input, output)
     char *key;
     char *col;
     char *lab;
     char *table;
     char *where;
     char *input;
     char *output;
{
    char SQL_stmt[QRY_LENGTH];
    char wherecl[QRY_LENGTH];
    int withlabel = 0;
    int i;

    memset(SQL_stmt, '\0', sizeof(SQL_stmt));


    if (where) {
	snprintf(wherecl, QRY_LENGTH, "%s and", where);
    }

    if (lab)
	withlabel = 1;

    if (withlabel) {
	snprintf(SQL_stmt, QRY_LENGTH,
		 "SELECT %s,%s,%s from %s where %s %s is not null and %s is not null order by %s,%s",
		 key, col, lab, table, wherecl, key, col, key, col);
    }
    else {
	snprintf(SQL_stmt, QRY_LENGTH,
		 "SELECT %s,%s from %s where %s %s is not null and %s is not null order by %s,%s",
		 key, col, table, wherecl, key, col, key, col);
    }

    i = runPg(SQL_stmt, input, output, withlabel);
    return (i);
}
