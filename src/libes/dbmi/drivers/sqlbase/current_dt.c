#include "globals.h"
#include <time.h>

/* convert the current date/time to SQLBASE internal date/time format */

/* NOTE: this function is to overcome a design flaw in the SQLBASE C/api
 *
 * When updating/inserting the CURRENT date/time into a date/time column
 * there is no mechanism, using bind, to request the CURRENT date/time
 * from the system. Therefore, I must get the curretn-time from Unix
 * and convert it to SQLBASE internal format
 *
 * Function returns 0 if OK, or an SQLTRCD return code, if the conversion
 * from a picture format to internal date/time fails. If this routine fails
 * it is probably because I didn't get it right
 */

current_datetime (dt, len)
    char *dt;     /* declared as char dt[SQLSDAT] */
    SQLTNML *len; /* needed when inserting/updating with bind */
{
    SQLTRCD rcd;
    char buf[1024];
    time_t clock;
    struct tm *tm;

    time(&clock);
    tm = localtime(&clock);

    strftime (buf, sizeof(buf), "%Y %m %d %H %M %S", tm);

    rcd = sqlxpd (dt, len, buf, "YYYY MM DD HH MI SS", NULL_TERMINATED);
    return rcd;
}
