/*
 *   d.grid
 *
 *   Draw the coordinate grid
 *   the user wants displayed on top of the current image.
 */

#include <stdlib.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

int plot_grid(double, double, double);
int plot_border(double, double, double);

int 
main (int argc, char **argv)
{
	int colorg, colorb ;
	double size ;
	double east, north ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4 ;
	struct Flag *nogrid, *noborder;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

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

	opt4 = G_define_option() ;
	opt4->key        = "bordercolor" ;
	opt4->type       = TYPE_STRING ;
	opt4->required   = NO;
	opt4->answer     = "brown" ;
	opt4->options    = D_color_list();
	opt4->description= "Sets the border color";

	nogrid = G_define_flag();
	nogrid->key = 'g';
	nogrid->description = "Disable grid drawing";

	noborder = G_define_flag();
	noborder->key = 'b';
	noborder->description = "Disable border drawing";


	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	colorg = D_translate_color(opt1->answer);
	if (colorg == 0)
		G_fatal_error ("Don't know the color %s", opt1->answer);

	colorb = D_translate_color(opt4->answer);
	if (colorb == 0)
		G_fatal_error ("Don't know the color %s", opt4->answer);

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

	if(!nogrid->answer)
	{
	  /* Set grid color */
	  R_standard_color(colorg) ;

	  /* Do the grid plotting */
	  plot_grid(size, east, north) ;
	}

	if(!noborder->answer)
	{
	  if (G_projection() == PROJECTION_LL)
	  	G_warning("border not yet implemented for LatLong locations: border not drawn.");
	  else
	  {
	    /* Set border color */
	    R_standard_color(colorb) ;
	  
	    /* Do the border plotting */
	    plot_border(size, east, north) ;
	  }
	}

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();

	exit(0);
}
