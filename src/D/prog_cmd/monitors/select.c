/* select a monitor for graphics */

#include <stdio.h>
#include "gis.h"
#include "monitors.h"

main(argc,argv) char *argv[];
{
    char file[100];
    struct MON_CAP *R_parse_monitorcap();

    if (argc != 2)
    {
        fprintf(stderr,"Usage:  %s monitor_name\n",argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    system ("Drelease.mon -v");
    G_unsetenv ("MONITOR");

    if (R_parse_monitorcap(MON_NAME,argv[1]) == NULL)
    {
	fprintf(stderr,"No such monitor as '%s'\n",argv[1]);
	exit(1);
    }

/* change the environment variable */
    G__setenv("MONITOR",argv[1]);

/* now try to run the monitor to see if it is running and to lock it
 * clear the screen, create a full screen window, set the font
 */
    R_open_driver();
    Dclearscreen();
    Dnew ("full_screen", 0., 100., 0., 100.);
    Dchoose ("full_screen") ;
    R_font ("romans");
    R_close_driver();

/* write the name to the .gisrc file */
    G__write_env();
    exit(0);
}
