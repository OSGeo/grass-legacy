#include <string.h>
#include "gis.h"
#include "global.h"
/***************************************************
 * these routines determine the printf format used
 * by floating point values
 *
 * format_parms() is called for each value.
 *     before first call set eformat=0,dp=6
 *
 * format_double() does the formating with the
 *     parms determined by format_parms()
 ***************************************************/

int 
format_parms (double v, int n, int *eformat, int *dp)
{
    char buf[50];

    for(;;)
    {
	format_double (v, buf, n, *eformat, *dp);
	if (strlen(buf) <= n) break;
	if(*dp)
	    *dp -= 1;
	else if (*eformat)
	    break;	/* OOPS */
	else
	{
	    *eformat = 1;
	    *dp = n;
	}
    }

    return 0;
}

int 
format_double (double v, char *buf, int n, int eformat, int dp)
{
    char fmt[15];

    if (eformat)
	sprintf (fmt, "%%%d.%de", n, dp);
    else
	sprintf (fmt, "%%%d.%dlf", n, dp);

    sprintf (buf, fmt, v);
    G_insert_commas (buf);

    return 0;
}
