#include <unistd.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#define MAIN
#include "options.h"
#include "local_proto.h"

struct Cell_head window ;

int 
main (int argc, char **argv)
{
	int color ;
	struct GModule *module;
	struct Option *opt1, *opt2/*, *opt3, *opt4*/ ;

	module = G_define_module();
	module->description =
		"Generates and displays simple graphics on map "
		"layers drawn in the active graphics monitor display frame.";

	opt1 = G_define_option() ;
	opt1->key        = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = NO ;
	opt1->description= "Unix file containg graphing instructions";

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options    = D_color_list();
	opt2->description= "Starting color desired for graphics" ;

/*
	opt3 = G_define_option() ;
	opt3->key        = "vsize" ;
	opt3->type       = TYPE_DOUBLE;
	opt3->answer     = "5.0" ;
	opt3->options    = "0-100" ;
	opt3->description= "Vertical text height as % of display frame height" ;

	opt4 = G_define_option() ;
	opt4->key        = "hsize" ;
	opt4->type       = TYPE_DOUBLE;
	opt4->answer     = "5.0" ;
	opt4->options    = "0-100" ;
	opt4->description= "Horizontal text width as % of display frame width" ;
*/

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	if (opt1->answer != NULL)
	{
		/* 1/4/91  jmoorman
		mapset = G_find_file ("mapgraph", opt1->answer, "");
		if (mapset == NULL)
		{
			fprintf (stdout,"Mapgraph file [%s] not available", opt1->answer);
			G_usage() ;
			exit(-1) ;
		}
		Infile = G_fopen_old ("mapgraph", opt1->answer, mapset);
		if (Infile == NULL)
		{
			fprintf (stdout,"Graph file <%s> not available\n", opt1->answer);
			G_usage() ;
			exit(-1) ;
		}
		*/
		/* using fopen instead to facilitate finding the file */
		if ((Infile = fopen(opt1->answer,"r")) == NULL) 
		    {
			fprintf (stdout,"Mapgraph file [%s] not available", opt1->answer);
			G_usage() ;
			exit(-1) ;
		}
	}
	else
	{
		Infile = stdin ;
		if (isatty(0))
			fprintf (stdout,"\nEnter mapgraph commands; terminate with a ^D\n\n") ;
	}
	color = D_translate_color(opt2->answer) ;

	/*
	sscanf(opt3->answer,"%lf",&temp);
	vsize = temp ;

	sscanf(opt4->answer,"%lf",&temp);
	hsize = temp ;
	*/

	vsize = hsize = 5.0 ;

	R_open_driver();

	D_setup(0);

	G_get_set_window(&window) ;

	R_standard_color(color) ;
	R_move_abs(
	    (int)(D_get_d_west() + D_get_d_east() / 2.0),
	    (int)(D_get_d_north() + D_get_d_south() / 2.0)) ;
	set_text_size() ;

	/* Do the graphics */
	G_setup_plot (
	    D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	    D_move_abs, D_cont_abs);

	graphics () ;

	/* Add this command to list */
	/*
	if(argc > 1)
	{
		D_add_to_list(G_recreate_command()) ;
	}
	*/

	R_close_driver();
	exit (0);
}
