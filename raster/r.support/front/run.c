#include <unistd.h>
#include <stdlib.h>
#include "gis.h"


/* 
 * run_etc_support() - Run command in etc/support 
 *
 * RETURN: >0 success : 0 failure
 */
int run_etc_support(char *pgm, char *rast)
{
    char buf[1024];
    int stat;

    snprintf(buf, sizeof(buf), "%s/etc/support/%s '%s'",
             G_gisbase(), pgm, rast);

    if ((stat = G_system(buf)))
	sleep(3);

    return stat;
}


/* 
 * run_system() - Run system command 
 *
 * RETURN: >0 success : 0 failure
 */
int run_system(char *pgm)
{
    char buf[1024];
    int stat;

    snprintf(buf, sizeof(buf), "%s", pgm);
    if ((stat = G_system(buf)))
	sleep(3);

    return stat;
}
