#include "what.h"
#include <stdio.h>
#include <string.h>

int buildPg(struct Option *ktab, struct Option *keycat, int curcat, char *print_out)

{
    int stat = 0;
    char SQL_stmt[QRY_LENGTH];

    memset(SQL_stmt, '\0', sizeof(SQL_stmt));

    snprintf(SQL_stmt, QRY_LENGTH,
	     "SELECT * from %s where %s=%d", ktab->answer, keycat->answer,
	     curcat);

    stat = runPg(SQL_stmt, print_out);

    return (stat);

}
