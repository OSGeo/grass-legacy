/*
 *   d.grid
 *
 *   Draw the coordinate grid
 *   the user wants displayed on top of the current image.
 */

#include "gis.h"
#include "display.h"
#include "raster.h"

int plot_grid(double, double, double);

int 
main (int argc, char **argv)
{
	int color ;
	double size ;
	double east, north ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3 ;

	module = G_define_module();
	module->description =
		"Overlays a user-specified grid "
		"in the active display frame on the graphics monitor.";

	opt2 = G_define_option() ;
	opt2->key        = "size" ;
	opt2->key_desc   = "value" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES;
	opt2->description= "Size of grid to be drawn" ;

	opt1 = G_define_option() ;
	opt1->key        = "color" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO;
	opt1->answer     = "gray" ;
	opt1->options    = D_color_list();
	opt1->description= "Sets the current grid color";

	opt3 = G_define_option() ;
	opt3->key        = "origin" ;
	opt3->type       = TYPE_STRING ;
	opt3->key_desc   = "easting,northing" ;
	opt3->answer     = "0,0" ;
	opt3->multiple   = NO;
	opt3->description= "Lines of the grid pass through this coordinate" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	color = D_translate_color(opt1->answer);
	if (color == 0)
	{
		fprintf (stdout,"Don't know the color %s\n", opt1->answer);
		exit(-1);
	}

	if(!G_scan_resolution (opt2->answer, &size, G_projection()) || size <= 0.0)
	{
		fprintf (stdout,"Invalid grid size <%s>\n", opt2->answer);
		exit(-1);
	}

	if(!G_scan_easting(opt3->answers[0], &east, G_projection()))
	{
		fprintf (stderr, "Illegal east coordinate <%s>\n",
		    opt3->answers[0]);
		G_usage();
		exit(1);
	}
	if(!G_scan_northing(opt3->answers[1], &north, G_projection()))
	{
		fprintf (stderr, "Illegal north coordinate <%s>\n",
		    opt3->answers[1]);
		G_usage();
		exit(1);
	}

	/* Setup driver and check important information */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	D_setup(0);

	/* Set color */
	R_standard_color(color) ;

	/* Do the plotting */
	plot_grid(size, east, north) ;

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();

	exit(0);
}
