#include <stdio.h>
#include "gis.h"
#include "monitors.h"
#include "open.h"

main(argc,argv) char *argv[];
{
    struct MON_CAP *cap;
    struct MON_CAP *R_parse_monitorcap();
    char *status;
    char *fmt = "%-15s %-30s %s\n";

    printf (fmt, "name","description","status");
    printf (fmt, "----","-----------","------");
    while ((cap = R_parse_monitorcap(MON_NEXT,"")) != NULL)
    {
        G__setenv("MONITOR",cap->name);
        R__open_quiet();
        switch(R_open_driver())
        {
        case OK:
            status = "running";
            R_close_driver();
            R_release_driver();
            break;
        case NO_RUN:
            status = "not running";
            break;
        case LOCKED:
            status = "in use";
            break;
        default:
            status = "??";
            break;
        }                   /* switch */
        printf(fmt,cap->name,cap->comment,status);
    }
}
