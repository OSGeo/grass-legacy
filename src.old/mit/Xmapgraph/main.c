/*  %W%  %G%  */

/*
 *   Xmapgraph
 *
 *   Usage:  Xmapgraph [input] [color] [hsize] [vsize]
 *           Xmapgraph [input=name] [color=name] [hsize=num] [vsize=num]
 *
 *   Draw graphics in a map window.
 */

#define MAIN
#include "options.h"
#include "gis.h"

#define USAGE	"[input=name] [color=name] [hsize=num] [vsize=num]"
struct Cell_head window ;

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	extern int stash_away() ;
	int i ;

/* Initialize the GIS calls */
	G_gisinit("Xmapgraph") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}


/* Read in the map window associated with window */
	G_get_window(&window) ;

/* Determine conversion factors 
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;
*/

	prepare();

	set_text_size() ;

/* Do the graphics */
	graphics (infile) ;

/* Add this command to list 
	if(argc > 1)
	{
		strcpy(buff, argv[0]) ;
		for(i=1; i<argc; i++)
		{
			strcat(buff, " ") ;
			strcat(buff, argv[i]) ;
		}
		D_add_to_list(buff) ;
	}
*/

}
