/*
 *   d.graph
 *
 *   Draw graphics in a graphics window.   Graph lines come from stdin,
 *      unless input specified.
 *
 */

#define MAIN
#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "colors.h"
#include "glocale.h"
#include "options.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
	char window_name[64] ;
	struct GModule *module;
	struct Option *opt1, *opt2 ;
	int R, G, B, color = 0;
	const int customcolor = MAX_COLOR_NUM + 1;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		_("Program for generating and displaying simple graphics to the "
		"graphics display monitor.");

	opt1 = G_define_option() ;
	opt1->key        = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO;
	opt1->description= _("Name of file containing graphics commands, "
			   "if not given reads from standard input");
	opt1->gisprompt  = "file,file,file";

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = NO;
	opt2->description= _("Color to draw with, either a standard GRASS color "
			   "or R:G:B triplet (separated by colons)");
	opt2->answer     = DEFAULT_FG_COLOR;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(1);

	hsize = vsize = 5. ;

	if (opt1->answer != NULL)
	{
	/* changed 1/4/91 jmoorman
		infile = G_fopen_old ("graph", opt1->answer, 
			G_find_file("graph", opt1->answer, "")) ;
		if (infile == NULL)
	*/
	    if ((infile = fopen(opt1->answer,"r")) == NULL)
		{
			G_usage();
			G_fatal_error ("Graph file <%s> not found",opt1->answer);
		}
	}
	else
		infile = stdin ;

	/* Parse and select color */
	if (opt2->answer != NULL) {
	    color = G_str_to_color(opt2->answer, &R, &G, &B);
	    if(color == 0)
		G_fatal_error("[%s]: No such color", opt2->answer);
	    if(color == 1) {
		R_reset_color(R, G, B, customcolor);
		R_color(customcolor);
	    }
	    /* (color==2) is "none", noop */
	}

	/* open graphics window */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;

	/* Finish graphics setup */
	R_set_window(t, b, l, r) ;

	/* Do the graphics */
	set_graph_stuff() ;
	set_text_size() ;
	graphics (infile) ;

	R_close_driver();

	exit(0);
}
