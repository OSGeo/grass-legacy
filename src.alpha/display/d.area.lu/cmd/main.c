/*
 *   d.vect
 *
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#include "gis.h"
#include "Vect.h"
#define MAIN

struct Cell_head window;
int linecolor;
main(argc, argv)
int argc ;
char **argv ;
{
	char *mapset ;
	char *color_mapset;
	char buf[128] ;
	char window_name[64];
	int i, stat ;
	int catval = 0;
	int t,b,l,r;
	char map_name[128],filename[128], color_name[128];
	FILE *cf;
	struct Option *opt1,*opt2, *opt3;
	struct line_pnts *Points;
	struct Colors colors;

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->multiple   = NO ;
	opt1->description= "Name of existing vector map to be displayed" ;

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = NO ;
	opt2->multiple   = NO ;
	opt2->description= "Name of colortable " ;

	opt3 = G_define_option();
	opt3->key	= "linecolor";
	opt3->type	= TYPE_STRING;
	opt3->answer	= "none";
	opt3->required	= NO;
	opt3->description= "Color for drawing lines";

	G_gisinit(argv[0]);

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	linecolor = D_translate_color(opt3->answer);

	strcpy(map_name, opt1->answer);

	if (opt2->answer == NULL) 
		strcpy(color_name, opt1->answer);
	else
		strcpy(color_name, opt2->answer);

	/* Make sure map is available */
	mapset = G_find_file2 ("dig", map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buf,"Vector file [%s] not available", map_name);
		G_fatal_error(buf) ;
		exit(-1);
	}

	color_mapset=G_find_file("colr",color_name,"");

	sprintf(buf,"Colortable %s not found",color_name);
	if (color_mapset == NULL)
		G_fatal_error(buf);

	if (G_read_colors(color_name, color_mapset, &colors) == -1)
		G_fatal_error("Colortable not available");

	R_open_driver();


	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window") ;
 
	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;
 
/* Read in the map window associated with window */
	G_get_window(&window) ;
 
	if (D_check_map_window(&window))
		G_fatal_error("Setting map window") ;
 
	if (G_set_window(&window) == -1)
		G_fatal_error("Current graphics window not settable") ;
 
/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;
   
	D_set_colors(&colors);
	Points = Vect_new_line_struct ();

	stat = plot (map_name, mapset, Points, &colors);
	if(stat == 0)
		D_add_to_list(G_recreate_command()) ;

	Vect_destroy_line_struct (Points);

	R_close_driver();
	exit(stat);
}

/* NULL function to bypass debugf() in dig library */
debugf() {}
