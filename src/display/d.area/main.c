/*
 *   d.area
 *   To obtain area/perimeter information on vector polygons
 *
 */

#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "local_proto.h"

#define MAIN
struct Cell_head window;
int fillcolor, linecolor;

int 
main (int argc, char **argv)
{
	char window_name[64];
	char *mapset ;
	char buf[128] ;
	int i, stat ;
	int t,b,l,r;
	char map_name[128];
	struct GModule *module;
	struct Option *opt1,*opt2, *opt3;
	struct line_pnts *Points;

	module = G_define_module();
	module->description =
		"Obtains area/perimeter information on vector polygons.";

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->multiple   = NO ;
	opt1->gisprompt  = "old,dig,vector" ;
	opt1->description= "Name of existing vector map to be displayed" ;

	opt2 = G_define_option() ;
	opt2->key        = "fillcolor" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options    = D_color_list();
	opt2->description= "Color desired for filling polys" ;

	opt3 = G_define_option() ;
	opt3->key        = "linecolor" ;
	opt3->type       = TYPE_STRING ;
	opt3->answer     = "none" ;
	opt3->description= "Color desired for drawing map" ;
	G_gisinit(argv[0]);

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer);
	fillcolor = D_translate_color(opt2->answer);
	linecolor = D_translate_color(opt3->answer);

	/* Make sure map is available */
	mapset = G_find_file2 ("dig", map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buf,"Vector file [%s] not available", map_name);
		G_fatal_error(buf) ;
		exit(-1);
	}

	
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphocs window");
	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available");
	G_get_window(&window) ;
	if (D_check_map_window(&window))
		G_fatal_error("Setting map window");
	if (G_set_window(&window) == -1)
		G_fatal_error("Current graphics window not settable");
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window");
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error doing conversions");

	Points = Vect_new_line_struct ();

	stat = plot1 (map_name, mapset, Points);

	if(stat == 0)
		D_add_to_list(G_recreate_command()) ;

	Vect_destroy_line_struct (Points);

	R_close_driver();
	exit(stat);
}

/* NULL function to bypass debugf() in dig library */
int debugf (void) {return 0;}
