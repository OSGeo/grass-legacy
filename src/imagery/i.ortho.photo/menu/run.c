#include "orthophoto.h"

run_etc_imagery (pgm, group)
    char *pgm;
    char *group;
{
    char buf[1024];
    int stat;

    /* Eventually this programs should live in $GIS/etc/imagery */
    sprintf (buf, "%s/etc/imagery/%s  %s", G_gisbase(), pgm, group);

    /* but for now they live in bin  */
    /* sprintf (buf, "%s %s", pgm, group);  */


    if(stat=G_system (buf))
	sleep(3);
    /* return ((int) stat);   */
}

run_system (pgm)
    char *pgm;
{
    char buf[1024];
    int stat;

    sprintf (buf, "%s", pgm);
    if(stat=G_system (buf))
	sleep(3);
    return ((int) stat);
}

