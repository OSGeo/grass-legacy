#define GLOBAL
#include "globals.h"
main()
{
    char dt[SQLSDAT];
    SQLTNML len;
    char buf[1024];

    current_datetime (dt, &len);

    sqlxdp (buf, sizeof buf, dt, len, "Mon DD, YYYY HH:MI:SS.999999", 0);
    printf ("%s\n", buf);
}
