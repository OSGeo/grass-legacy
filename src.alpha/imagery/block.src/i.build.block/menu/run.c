#include "dba_imagery.h"

run (pgm, block)
    char *pgm;
    char *block;
{
    char buf[1024];
    int stat;

    sprintf (buf, "%s/etc/imagery/%s %s", G_gisbase(), pgm, block);
    if(stat=G_system (buf))
	sleep(3);
    return stat;
}
