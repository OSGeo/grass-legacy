/* select a monitor for graphics */

#include <stdio.h>
#include "gis.h"
#include "monitors.h"

main(argc,argv) char *argv[];
{
	struct MON_CAP *R_parse_monitorcap();
	char command[1024];

	if (argc != 2)
	{
		fprintf(stderr,"Usage:  %s monitor_name\n",argv[0]);
		exit(1);
	}

	G_gisinit (argv[0]);
    /* users objected to automatic release of current MONITOR
	sprintf (command, "%s/etc/mon.release -v", G_gisbase());
	system (command);
    */
	G_unsetenv ("MONITOR");

	if (R_parse_monitorcap(MON_NAME,argv[1]) == NULL)
	{
		fprintf(stderr,"No such monitor as '%s'\n",argv[1]);
		exit(1);
	}

	/* change the environment variable */
	G__setenv("MONITOR",argv[1]);

/* now try to run the monitor to see if it is running and to lock it
 * set the font
 * if no current frame create a full screen window.
 */
	R_open_driver();
	R_font ("romans");
	D_setup(0);
	R_close_driver();

	/* write the name to the .gisrc file */
	G__write_env();
	exit(0);
}
