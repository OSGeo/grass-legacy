#include "datetime.h"

main()
{
    DateTime dt;
    int tz;
    char buf[1024];

    datetime_get_local_time (&dt);
    datetime_get_local_timezone (&tz);
    datetime_set_timezone (&dt, tz);
    datetime_format (&dt, buf);

    fprintf (stdout,"<%s>\n", buf);
}
