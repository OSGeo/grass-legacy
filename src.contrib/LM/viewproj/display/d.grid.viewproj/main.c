/*
****************************************************************************
*
* MODULE:       d.grid.viewproj
* AUTHOR(S):    Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a grid in a map projection.
*		Based upon d.grid
* COPYRIGHT:    (C) 2003 by Lockheed Martin Space Systems, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/


#include "gis.h"
#include "display.h"
#include "raster.h"

#include "coord_systems.viewproj.h"


int plot_grid_viewproj (double, double, double);

int 
main (int argc, char **argv)
{
	int color ;
	double size ;
	double east, north ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3 ;
	int dummy; /* dummy variable */

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Overlays a user-specified grid "
		"in the view projection on the graphics monitor.";

	opt2 = G_define_option() ;
	opt2->key        = "size" ;
	opt2->key_desc   = "value" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES;
	opt2->description= "Size in degrees of grid to be drawn" ;

	opt1 = G_define_option() ;
	opt1->key        = "color" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO;
	opt1->answer     = "gray" ;
	opt1->options    = D_color_list();
	opt1->description= "Sets the grid color";

	opt3 = G_define_option() ;
	opt3->key        = "origin" ;
	opt3->type       = TYPE_STRING ;
	opt3->key_desc   = "easting,northing" ;
	opt3->answer     = "0,0" ;
	opt3->multiple   = NO;
	opt3->description= "Lines of the grid pass through this coordinate" ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	color = D_translate_color(opt1->answer);
	if (color == 0)
		G_fatal_error ("Don't know the color %s", opt1->answer);

	if(!G_scan_resolution (opt2->answer, &size, G_projection()) || size <= 0.0)
		G_fatal_error ("Invalid grid size <%s>", opt2->answer);

	if(!G_scan_easting(opt3->answers[0], &east, G_projection()))
	{
		G_usage();
		G_fatal_error ("Illegal east coordinate <%s>",
		    opt3->answers[0]);
	}
	if(!G_scan_northing(opt3->answers[1], &north, G_projection()))
	{
		G_usage();
		G_fatal_error ("Illegal north coordinate <%s>",
		    opt3->answers[1]);
	}

	/* Setup driver and check important information */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	D_setup(0);

        setup_conversions_viewproj (0, &dummy, &dummy, &dummy, &dummy);

	/* Set color */
	R_standard_color(color) ;

	/* Do the plotting */
	plot_grid_viewproj (size, east, north) ;

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();

	free_conversions_viewproj(); /* Bev Wallace - free all PROJ memory*/

	exit(0);
}
