/*
 *   d.graph
 *
 *   Draw graphics in a graphics window.   Graph lines come from stdin,
 *      unless input specified.
 *
 */

#define MAIN
#include <stdio.h>
#include "options.h"
#include "gis.h"

main(argc, argv)
int argc ;
char **argv ;
{
	char window_name[64] ;
	char *D_color_list();
	extern int stash_away() ;
	struct Option *opt1, *opt2 ;
	FILE *G_fopen_old() ;

	opt1 = G_define_option() ;
	opt1->key        = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO;
	opt1->description= "Name of file containing graphics command" ;

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options    = D_color_list();
	opt2->description= "Color selection graphics" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	hsize = vsize = 5. ;

	/* Check command line */

	if (G_parser(argc, argv))
		exit(-1);

	if (opt1->answer != NULL)
	{
	/* changed 1/4/91 jmoorman
		infile = G_fopen_old ("graph", opt1->answer, 
			G_find_file("graph", opt1->answer, "")) ;
		if (infile == NULL)
	*/
	    if ((infile = fopen(opt1->answer,"r")) == NULL)
		{
			printf("Graph file <%s> not found\n",opt1->answer);
			G_usage() ;
			exit(-1) ;
		}
	}
	else
		infile = stdin ;

	if (opt2->answer != NULL)
	{
		color = D_translate_color(opt2->answer) ;
		if (color == 0)
		{
			printf("Don't know the color %s\n", opt2->answer) ;
			exit(-1);
		}
	}

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;

	/* Finish graphics setup */
	R_set_window(t, b, l, r) ;
	R_standard_color(color) ;

	/* Do the graphics */
	set_graph_stuff() ;
	set_text_size() ;
	graphics (infile) ;

	R_close_driver();
}
