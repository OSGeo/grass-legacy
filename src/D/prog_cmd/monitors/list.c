/* list.mon - list entries in monitorcap file */

#include <stdio.h>
#include "monitors.h"

main(argc,argv) char *argv[];
{
    struct MON_CAP *cap;
    struct MON_CAP *R_parse_monitorcap();
    int n ;

    n=0 ;
    while ((cap = R_parse_monitorcap(MON_NEXT,"")) != NULL)
    {
        n++ ;
        if (*(cap->tty) != '\0')
            printf ("     %s - %s (%s)\n",cap->name,cap->comment,cap->where);
        else
            printf ("     %s - %s\n",cap->name,cap->comment);
    }
    if (!n)
        printf ("     no known mointors\n") ;
}
