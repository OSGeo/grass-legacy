#include "gis.h"
#define MAIN
#include "options.h"
/* 3d program gathers input for D3d which is then executed */

main(argc,argv) char *argv[];
{
	char window_name[64] ;
	int do_it ;
	char erase_color[64] ;
	char *mapset ;

	G_gisinit(argv[0]) ;
	R_open_driver();
	G_clear_screen() ;

/* Get name of files for display */
	printf("\n        3-D Landscape display\n") ;
	printf("\nFirst enter the names of the maps to be used to") ;
	printf("\nfor color and for elevation:\n\n") ;
	mapset =
		G_ask_cell_old("Enter raster file to be displayed: ", file) ;
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
	    R_open_driver();
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

		printf("\n3-d view request:\n") ;
		printf("File plotted:    %s in %s\n", file, file_mapset) ;
		printf("Elevation file:  %s in %s\n", elevfile, elevfile_mapset) ;
		printf("\n") ;
		printf("            View from:    View to:\n") ;
		printf("Easting:  %12.2lf %12.2lf\n", from_easting, to_easting) ;
		printf("Northing: %12.2lf %12.2lf\n", from_northing, to_northing) ;
		printf("Height:   %12.2lf %12.2lf\n", from_height, to_height) ;
		printf("\n") ;
		printf("line frequency:         %d\n", line_freq) ;
		printf("elevation exaggeration: %4.2lf\n", exag) ;
		printf("field of view:          %4.2lf\n", field) ;

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
}
