#include <stdio.h>
#include "gis.h"
#include "monitors.h"
#include "open.h"

main(argc,argv) char *argv[];
{
    struct MON_CAP *cap;
    struct MON_CAP *R_parse_monitorcap();
    char *status;

    while ((cap = R_parse_monitorcap(MON_NEXT,"")) != NULL)
    {
        G__setenv("MONITOR",cap->name);
        R__open_quiet();
        switch(R_open_driver())
        {
        case OK:
            status = "idle";
            R_close_driver();
            R_release_driver();
            break;
        case NO_RUN:
            status = "not loaded";
            break;
        case LOCKED:
            status = "in use";
            break;
        default:
            status = "in an indeterminate state";
            break;
        }                   /* switch */
        printf("%s (%s) is %s\n",cap->name,cap->comment,status);
    }
}
