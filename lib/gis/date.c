#include "gis.h"
/*
 **********************************************************************
 *
 *   char *
 *   G_date ()
 *
 *   returns pointer to a string containing the current date and time
 *
 **********************************************************************/

#include <time.h>

char *
G_date()
{
    long clock;
    struct tm *local;
    char *date;
    char *d;

    char *asctime();
    struct tm *localtime();

    time(&clock);

    local = localtime(&clock);
    date = asctime(local);
    for (d = date; *d; d++)
	if (*d == '\n')
	    *d = 0;
    return date;
}
