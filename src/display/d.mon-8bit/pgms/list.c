/* list.mon - list entries in monitorcap file */

#include <stdio.h>
#include "monitors.h"

main(argc,argv) char *argv[];
{
    struct MON_CAP *cap;
    struct MON_CAP *R_parse_monitorcap();
    int n ;
    char *fmt1 = "%-15s %-30s\n";
    char *fmt2 = "%-15s %-30s (%s)\n";

    n=0 ;
    while ((cap = R_parse_monitorcap(MON_NEXT,"")) != NULL)
    {
        if(n++ ==0)
	{
	    printf (fmt1, "name","description");
	    printf (fmt1, "----","-----------");
	}
        if (*(cap->tty) != '\0')
            printf (fmt2,cap->name,cap->comment,cap->where);
        else
            printf (fmt1,cap->name,cap->comment);
    }
    if (!n)
        printf ("     no known monitors\n") ;
}
