/*
****************************************************************************
*
* MODULE:       d.vect.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To display a vector map in a map projection.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*************************************************************************
d.vect.viewproj

Draws the binary vector (dig) file that the user wants displayed on top of 
the current image.
Draws the vector map in the projection set by d.set.viewproj and d.mon.viewproj.

This file is a slightly modify version from main.c of d.vect (Grass 4.1)
I changed plot1 & plot 2 to plot1_viewproj & plot2_viewproj.
I also added the line "setup_conversions_viewproj(0);"

Sharif Razzaque June 1995
*************************************************************************
*/

#include <stdlib.h>
#include <string.h>

#include "config.h"	/* For Grass 5.0 Bev Wallace */
#include "gis.h"
#include "Vect.h"
#include "display.h"	/* For D_* - Bev Wallace */
#include "raster.h"	/* For R_* - Bev Wallace */

#include "coord_systems.viewproj.h" /* contains prototype & coordinate types */

extern int plot1_viewproj (char *name, char *mapset, struct line_pnts *Points);


int main (int argc, char **argv)
{
	char *mapset ;
	char buf[128] ;
	int stat ;
	int color;
	char *D_color_list();
	char map_name[128] ;
	struct Option *opt1, *opt2;
	struct Flag   *levone;
	struct line_pnts *Points;
	int t; /* dummy variable */

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

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;


	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer);

	color = D_translate_color(opt2->answer);

	/* Make sure map is available */
	mapset = G_find_file2 ("dig", map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buf,"Vector file [%s] not available", map_name);
		G_fatal_error(buf) ;
		exit(-1);
	}

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	D_setup(0);

        setup_conversions_viewproj(0,&t,&t,&t,&t);

	R_standard_color(color) ;

	Points = Vect_new_line_struct ();

	/* Bev Wallace - always use plot1 */
	stat = plot1_viewproj (map_name, mapset, Points);
/*
	if (use_plot1(map_name, mapset) || levone->answer )
		stat = plot1_viewproj (map_name, mapset, Points);
	else if (stat = plot2_viewproj(map_name, mapset, Points))
	{
		stat = plot1_viewproj (map_name, mapset, Points);
	}
*/
	if(stat == 0)
		D_add_to_list(G_recreate_command()) ;

	Vect_destroy_line_struct (Points);

	R_close_driver();

	free_conversions_viewproj(); /* Bev Wallace - free all PROJ memory*/
 
	exit(stat);
}


/* NULL function to bypass debugf() in dig library */
void debugf(void) {}
