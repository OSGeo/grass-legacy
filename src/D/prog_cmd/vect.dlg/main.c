/*  %W%  %G%  */

/*
 *   Dvect
 *
 *   Usage:  Dvect mapname [color]
 *           Dvect name=mapname color=name
 *
 *   Draw the binary digital line graph (bdlg) file that
 *   the user wants displayed on top of the current image.
 */

#define USAGE	"name=mapname [color=name]"
#include "gis.h"
#include "dlg.h"
#define MAIN
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	FILE *fd ;
	char *mapset ;
	char buff[128] ;
	char window_name[64] ;
	struct Cell_head window ;
	struct Colors colors ;
	struct dlg dlg ;
	int i ;
	extern int stash_away() ;
	int t, b, l, r ;

/* Initialize the GIS calls */
	G_gisinit("Dvect") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	mapset = G_find_file ("bdlg", map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Vector file [%s] not available", map_name);
		G_fatal_error(buff) ;
	}

    R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting map window") ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;

/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

/* Check for and read dlg header info */

	if (! (fd = G_fopen_old("bdlg", map_name, mapset) ) )
	{
		sprintf(buff, "Can't open bdlg file [%s] in [%s]",
			map_name, mapset) ;
		R_close_driver();
		G_fatal_error(buff) ;
	}

	if (dlg_init(fd, &dlg) == -1)
	{
		fclose(fd) ;
		R_close_driver();
		G_fatal_error("Problem in initial read of dlg file") ;
	}

	printf("\nSelected information from dlg header\n") ;
	printf(" Banner:      %s\n", dlg.head.banner) ;
	printf(" Cart. Unit:  %s\n", dlg.head.cart_unit) ;
	printf(" Source Date: %s\n", dlg.head.source_date) ;
	printf(" Orig. Scale: %s\n", dlg.head.orig_scale) ;

/* Do initial read of dlg file */
	printf("\nMaking initial read of dlg file... Please wait\n") ;
	dlg_read(fd, &dlg) ;
	printf("This file currently contains:\n") ;
	printf("  %d nodes, %d areas, and %d lines\n", 
		dlg.max_nodes, dlg.max_areas, dlg.max_lines) ;

/* Set color */
	R_standard_color(color) ;

/* Do the plotting */
	dlg_plot_all_lines(fd, &dlg) ;

	fclose(fd) ;

/* Add this command to list */
	strcpy(buff, argv[0]) ;
	for(i=1; i<argc; i++)
	{
		strcat(buff, " ") ;
		strcat(buff, argv[i]) ;
	}
	D_add_to_list(buff) ;

    R_close_driver();
}
