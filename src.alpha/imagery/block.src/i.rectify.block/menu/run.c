#include "dba_imagery.h"

run (pgm, block, subblock)
    char *pgm;
    char *block;
    char *subblock;
{
    char buf[1024];
    int stat;

    sprintf (buf, "%s/etc/imagery/%s %s %s", G_gisbase(), pgm, block, subblock);
    if(stat=G_system (buf))
	sleep(3);
    return stat;
}

