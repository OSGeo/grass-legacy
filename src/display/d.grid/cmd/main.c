/*   $Id$
 *   d.grid
 *
 *   Draw the coordinate grid
 *   the user wants displayed on top of the current image.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "colors.h"

int plot_grid(double, double, double);
int plot_border(double, double, double);

int 
main (int argc, char **argv)
{
	int colorg = 0;
	int colorb = 0;
	const int customGcolor = MAXCOLORS + 1;
	const int customBcolor = MAXCOLORS + 2;
	int R, G, B;
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
/*	opt1->options    = D_color_list(); */
	opt1->description=
	    "Sets the current grid color, either a standard GRASS color or R:G:B triplet (separated by colons)";

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
/*	opt4->options    = D_color_list(); */
	opt4->description=
	    "Sets the border color, either a standard GRASS color or R:G:B triplet";

	nogrid = G_define_flag();
	nogrid->key = 'g';
	nogrid->description = "Disable grid drawing";

	noborder = G_define_flag();
	noborder->key = 'b';
	noborder->description = "Disable border drawing";


	/* Check command line */
	if (G_parser(argc, argv))
		exit(1);

	/* Parse and select grid color */
	if(sscanf(opt1->answer, "%d:%d:%d", &R, &G, &B) == 3) {
	    if (R>=0 && R<256 && G>=0 && G<256 && B>=0 && B<256) {
		R_reset_color(R, G, B, customGcolor);
		colorg = customGcolor;
	    }
	}
	else
	    colorg = D_translate_color(opt1->answer);

	if(!colorg)
	    G_fatal_error("[%s]: No such color", opt1->answer);


	/* Parse and select border color */
	if(sscanf(opt4->answer, "%d:%d:%d", &R, &G, &B) == 3) {
	    if (R>=0 && R<256 && G>=0 && G<256 && B>=0 && B<256) {
		R_reset_color(R, G, B, customBcolor);
		colorb = customBcolor;
	    }
	}
	else
	    colorb = D_translate_color(opt4->answer);

	if(!colorb)
	    G_fatal_error("[%s]: No such color", opt4->answer);


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
	    if (colorg > MAXCOLORS)  /* ie custom RGB color */
		R_color(colorg);
	    else
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
		if (colorb > MAXCOLORS)  /* ie custom RGB color */
		    R_color(colorb);
		else
		    R_standard_color(colorb) ;
	  
		/* Do the border plotting */
		plot_border(size, east, north) ;
	    }
	}

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();

	exit(0);
}
