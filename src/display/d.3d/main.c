#include <string.h>
#include "display.h"
#include "D.h"
#include <stdlib.h>
#include "raster.h"
#include "gis.h"
#define MAIN
#include "options.h"
#include "l_proto.h"
/* 3d program gathers input for d.3d which is then executed */

int 
main (int argc, char *argv[])
{
	char window_name[64] ;
	int do_it ;
	char erase_color[64] ;
	char *mapset ;

	G_gisinit(argv[0]) ;
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics driver selected");

	G_clear_screen() ;

/* Get name of files for display */
	fprintf (stdout,"\n        3-D Landscape display\n") ;
	fprintf (stdout,"\nFirst enter the names of the maps to be used to") ;
	fprintf (stdout,"\nfor color and for elevation:\n\n") ;
	mapset =
		G_ask_cell_old("Enter raster file to be displayed (color): ", file) ;
	if(mapset == NULL)
		exit(0) ;
	else
		strcpy(file_mapset, mapset) ;

	mapset =
		G_ask_cell_old("Enter raster file to used for elevation: ", elevfile) ;
	if(mapset == NULL)
		exit(0) ;
	else
		strcpy(elevfile_mapset, mapset) ;


	/* Set up graphics */
	if (D_get_cur_wind(window_name))
	{
	    R_close_driver();
	    system ("d.frame -e");
	    if (R_open_driver() != 0)
		    G_fatal_error ("No graphics driver selected");
	}

	for(;;)
	{

		G_get_window(&window) ;

		get_inputs(&do_it, erase_color) ;
		if(! do_it)
			break ;

		check_options();

		if (0 > G_set_window(&window) )
			G_fatal_error("Inappropriate resolution request") ;

		fprintf (stdout,"\n3-d view request:\n") ;
		fprintf (stdout,"File plotted:    %s in %s\n", file, file_mapset) ;
		fprintf (stdout,"Elevation file:  %s in %s\n", elevfile, elevfile_mapset) ;
		fprintf (stdout,"\n") ;
		fprintf (stdout,"            View from:    View to:\n") ;
		fprintf (stdout,"Easting:  %12.2f %12.2f\n", from_easting, to_easting) ;
		fprintf (stdout,"Northing: %12.2f %12.2f\n", from_northing, to_northing) ;
		fprintf (stdout,"Height:   %12.2f %12.2f\n", from_height, to_height) ;
		fprintf (stdout,"\n") ;
		fprintf (stdout,"line frequency:         %d\n", line_freq) ;
		fprintf (stdout,"elevation exaggeration: %4.2f\n", exag) ;
		fprintf (stdout,"field of view:          %4.2f\n", field) ;

		G_get_set_window(&window) ;

                if(PROJECTION_LL == window.proj){
		    if(360. < (window.east - window.west + 2.*window.ew_res)){
			window.east  -= window.ew_res ;
			window.west  += window.ew_res ;
			window.cols -= 2 ;
		    }	
		    if(180. < (window.north-window.south + 2.*window.ns_res)){
			window.north -= window.ns_res ;
			window.south += window.ns_res ;
			window.rows -= 2 ;
		    }
		    if (0 > G_set_window(&window) )
			G_fatal_error("Inappropriate resolution request");
		}

		/* adjust window one extra row n, s, e, w */
		window.north += window.ns_res ;
		window.south -= window.ns_res ;
		window.east  += window.ew_res ;
		window.west  -= window.ew_res ;
		window.rows += 2 ;
		window.cols += 2 ;
		if (0 > G_set_window(&window) )
			G_fatal_error("Inappropriate resolution request") ;


		if(*erase_color)
			Derase(erase_color) ;

		if (D_get_cur_wind(window_name))
			G_fatal_error("No current graphics frame") ;

		if (D_set_cur_wind(window_name))
			G_fatal_error("Current graphics frame not available") ;

		establish_view(from_easting,from_northing,from_height,
			to_easting,to_northing,to_height,field) ;

		threed(1);
	}

	save_defaults() ;

	R_close_driver() ;

	return 0;
}
