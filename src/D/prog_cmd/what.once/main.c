/*  %W%  %G%  */
/*
 *   Dwhat
 *
 *   Usage:  Dwhat [layer]
 *
 */

#define GLOBAL
#include "what.h"

main(argc, argv)
int argc ;
char **argv ;
{
	struct Cell_head window ;
	char temp[128] ;
	int t, b, l, r ;
	int i;

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;

	R_open_driver();

	if (D_get_cur_wind(temp))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(temp))
		G_fatal_error("Current graphics window not available") ;

	/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting graphics window") ;

	if (G_set_window(&window) == -1)
		G_fatal_error("Can't set current graphics window") ;

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting graphics window coordinates") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

	if ((argc == 1) || (strcmp (argv[1], "-") == 0))
	{
		D_get_cell_name (temp);
		if (*temp == 0)
		{
			fprintf (stderr, "warning: no data layer drawn in current window\n");
			exit(-1) ;
		}
		else if ((fd = opencell (temp, name, mapset)) < 0)
		{
			fprintf (stderr, "warning: inappropriate data layer in current window\n");
			exit(-1) ;
		}
	}
	else if ((fd = opencell (argv[1], name, mapset)) < 0)
	{
		fprintf (stderr, "warning: data layer [%s] not found\n", argv[1]);
		exit(-1) ;
	}

	what();

	R_close_driver();
}
