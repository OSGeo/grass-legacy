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


/*!
 * \brief current date and time
 *
 * Returns a pointer to a string
 * which is the current date and time. The format is the same as that produced by
 * the UNIX <i>date</i> command.
 *
 *  \param ~
 *  \return char * 
 */

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
