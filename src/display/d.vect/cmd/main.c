/*
 *   d.vect
 *
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#define MAIN
#include "local_proto.h"
int quiet = 1;

int 
main (int argc, char **argv)
{
	char *mapset ;
	char buf[128] ;
	int stat;
	int color;
	char *D_color_list();
	char map_name[128] ;
	struct GModule *module;
	struct Option *opt1, *opt2;
	struct Flag   *levone,  *_quiet;
	struct line_pnts *Points;

	module = G_define_module();
	module->description =
		"Displays GRASS vector data in the active frame on the "
		"graphics monitor.";

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->multiple   = NO ;
	opt1->gisprompt  = "old,dig,vector" ;
	opt1->description= "Name of existing vector map to be displayed" ;

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options    = D_color_list();
	opt2->description= "Color desired for drawing map" ;

	levone = G_define_flag ();
	levone->key		= 'm';
	levone->description	= "Use less memory";

	_quiet = G_define_flag ();
	_quiet->key		= 'v';
	_quiet->description	= "Run verbosely";

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer);

	color = D_translate_color(opt2->answer);

	quiet = !_quiet->answer;
	/* Make sure map is available */
	mapset = G_find_file2 ("dig", map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buf,"Vector file [%s] not available", map_name);
		G_fatal_error(buf) ;
		exit(-1);
	}

	R_open_driver();

	D_setup(0);

	R_standard_color(color) ;

	Points = Vect_new_line_struct ();

	if (use_plot1(map_name, mapset) || levone->answer )
		stat = plot1 (map_name, mapset, Points);
	else if (stat = plot2 (map_name, mapset, Points))
	{
		/*
		fprintf (stderr, "\n*** Will try another method ***\n\n");
		*/
		stat = plot1 (map_name, mapset, Points);
	}
	if(stat == 0)
		D_add_to_list(G_recreate_command()) ;

	D_set_dig_name(G_fully_qualified_name(map_name, mapset));
	D_add_to_dig_list(G_fully_qualified_name(map_name, mapset));

	Vect_destroy_line_struct (Points);

	R_close_driver();
	exit(stat);
}
