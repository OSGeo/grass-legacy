/*
 *   d.area
 *   Displays filled polygons.
 *
 */

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "local_proto.h"
#include "colors.h"

#define MAIN
struct Cell_head window;
int fillcolor, linecolor;
int *mycatlist;

int 
main (int argc, char **argv)
{
	char window_name[64];
	char *mapset ;
	char buf[128] ;
	int i, stat ;
	int t,b,l,r;
	unsigned char red, green, blue;
	char map_name[128];
	struct GModule *module;
	struct Option *opt1,*opt2, *opt3, *opt4;
	struct Flag *colcycle;
	struct line_pnts *Points;
	char colorname[MAX_COLOR_LEN];
	int backgroundcol=D_translate_color("black"); /* default: black */

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Draws selected polygons to the GRASS display.\n"\
		"Colors may be:\n"\
		"\t*) An X11 color name from rgb.txt\n"\
		"\t*) A hexadecimal value -- \"FFDF60\")\n"\
		"\t*) An RGB triplet -- \"rgb(127 242 93)\"\n";

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
	/* opt2->options    = D_color_list(); */
	opt2->description= "Color desired for filling polys" ;

	opt3 = G_define_option() ;
	opt3->key        = "linecolor" ;
	opt3->type       = TYPE_STRING ;
	opt3->answer     = "none" ;
	opt3->description= "Color desired for drawing map" ;

	opt4 = G_define_option();
	opt4->key         = "catnum" ;
	opt4->type        = TYPE_INTEGER;
	opt4->required    = NO;
	opt4->multiple    = YES;
	opt4->description = "List of area category number(s) to display" ;

	colcycle = G_define_flag ();
	colcycle->key		= 'f';
	colcycle->description	= "Fill all areas with color (cycle through standard colors)";

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer);

	/* Make sure map is available */
	mapset = G_find_file2 ("dig", map_name, "") ;
	if (mapset == NULL)
		G_fatal_error("Vector file [%s] not available", map_name) ;

	/* See about category list */
	if (opt4->answers != NULL && opt4->answers[0] != NULL)
	{
		for (i = 0; opt4->answers[i] != NULL; i++)
			;
		
		mycatlist = (int *) G_malloc (sizeof (int) * i + 1);
		if (mycatlist == NULL)
			G_fatal_error ("Memory Allocation Failed!");
		
		for (i = 0; opt4->answers[i] != NULL; i++)
		{
			mycatlist[i] = atoi (opt4->answers[i]);
			if (mycatlist[i] <= 0)
				G_fatal_error (
					"Integer conversion of category [%s] failed",
					opt4->answers[i]);
		}
		mycatlist[i] = 0;
	}
	else
	{
		mycatlist = NULL;
	}
	
	
	if (R_open_driver() != 0)
		G_fatal_error("No graphics driver selected");

	if (colcycle->answer)
	{ 
	 /* get backgroundcol of monitor to omit this color when filling areas */
	 if (D_get_erase_color(colorname) != 0)
	 {      /* this sometimes happens after d.rast */
	 	fprintf(stderr, "Can't get background color of monitor, using black\n");
	 	strcpy(colorname, "black");
	 }
	 backgroundcol=D_translate_color(colorname);
	 
	 if (strcmp("white", opt2->answer))
         {
	    fprintf(stderr, "Ignoring fillcolor settings due to -f\n");
	 }
	 fillcolor = 1; /* don't need fillcolor now, set to something */
	}
	else
	{
		if (color_parse (opt2->answer, &red, &green, &blue) == 0)
		{
			fillcolor = 14;
			R_reset_color(red, green, blue, fillcolor);
		}
		else
		{
			G_fatal_error ("Failed color lookup for \"%s\"",
					opt2->answer);
		}
	}
	
	linecolor = 0; /* no lines */
	if (strcmp(opt3->answer,"none"))
	{
		if (color_parse (opt3->answer, &red, &green, &blue) == 0)
		{
			linecolor = 15;
			R_reset_color(red, green, blue, linecolor);
		}
		else
		{
			G_fatal_error (
				"Failed color lookup for \"%s\"", 
				opt3->answer);
		}
	}

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window");
	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available");
	G_get_window(&window) ;
	if (D_check_map_window(&window))
		G_fatal_error("Setting map window");
	if (G_set_window(&window) == -1)
		G_fatal_error("Current graphics window not setable");
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window");
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error doing conversions");

	Points = Vect_new_line_struct ();

	stat = plot1 (map_name, mapset, Points, colcycle->answer, backgroundcol);

	if(stat == 0)
	    D_add_to_list(G_recreate_command()) ;
	
        D_set_dig_name(G_fully_qualified_name(map_name, mapset));
        D_add_to_dig_list(G_fully_qualified_name(map_name, mapset));

	Vect_destroy_line_struct (Points);

	R_close_driver();
	exit(stat);
}

/* NULL function to bypass debugf() in dig library */
int debugf (void) {return 0;}
