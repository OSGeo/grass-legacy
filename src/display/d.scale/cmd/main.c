/*
 * $Id$
 *********************************************************************
 *
 * MODULE:     d.scale
 * AUTHOR(S):  original author: James Westervelt (CERL)
 *             added -i scale flag: Markus Neteler 3/2001
 *
 * PURPOSE:    display the map scale
 * COPYRIGHT:  (C) 2001 by the GRASS Development Team
 *
 *             This program is free software under the GNU General Public
 *             License (>=v2). Read the file COPYING that comes with GRASS
 *             for details.
 *
 *********************************************************************/
   

#include "gis.h"
#include "display.h"
#include "raster.h"
#define MAIN
#include "options.h"

int draw_scale(int);
int screenscale(void);

int main (int argc, char **argv)
{
	char window_name[255] ;
	struct Cell_head window ;
	int t, b, l, r ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3 ;
	struct Flag *mouse, *screenscale;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Overlays a bar scale and north arrow for "
		"the current geographic region at a user-defined "
		"location in the active display frame.";


	opt1 = G_define_option() ;
	opt1->key        = "bcolor" ;
	opt1->type       = TYPE_STRING ;
	opt1->answer     = "black" ;
	opt1->required   = NO ;
	opt1->options    = D_color_list();
	opt1->description= "Color used for the background" ;

	opt2 = G_define_option() ;
	opt2->key        = "tcolor" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->required   = NO ;
	opt2->options    = D_color_list();
	opt2->description= "Color used for the text" ;

	opt3 = G_define_option() ;
	opt3->key        = "at";
	opt3->key_desc   = "x,y";
	opt3->type       = TYPE_DOUBLE;
	opt3->answer     = "0.0,0.0";
	opt3->options    = "0-100" ;
	opt3->required   = NO;
	opt3->description= "the screen coordinates for top-left corner of label" ;

	mouse = G_define_flag() ;
	mouse->key        = 'm';
	mouse->description= "Use mouse to interactively place scale" ;

	screenscale = G_define_flag() ;
	screenscale->key        = 'i';
	screenscale->description= "Display map scale in GRASS monitor and exit" ;

	coord_inp = 0;

	if (G_parser(argc, argv) < 0)
		exit(-1);

	color1 = D_translate_color(opt1->answer) ;

	color2 = D_translate_color(opt2->answer) ;

	/*
	G_scan_easting(opt3->answers[0], &easting, G_projection());
	coord_inp++;
	G_scan_northing(opt3->answers[1], &northing, G_projection());
	coord_inp++;
	*/
	sscanf(opt3->answers[0],"%lf",&east) ;
	sscanf(opt3->answers[1],"%lf",&north) ;
	if((east>0)||(north>0)) coord_inp=1;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	else
	{
	  if (screenscale->answer)  /* display map scale, added MN */
	  {
	  	screeninfo();
  	        R_close_driver();
	  	exit(0);
	  }
	}

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available");

	/* Read in the map window associated with window */
	G_get_window(&window);

	if (D_check_map_window(&window))
		G_fatal_error("Setting map window");

	if (G_set_window(&window) == -1)
		G_fatal_error("Current window not settable");

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

	/* Draw the scale */
	draw_scale(mouse->answer) ;

	/* Add this command to list */
	if (! mouse->answer)
		D_add_to_list(G_recreate_command()) ;

	R_close_driver();

	exit(0);
}
