
/* main.c 
 *
 * function defined:
 *
 * main for program Dhistogram 
 *
 *
 * PURPOSE: To draw a bar-chart or a pie-chart representing the
 * histogram statistics of a cell-file
 *
 * Usage: 
 *
 *    Dhistogram mapname [color]
 *    Dhistogram name=mapname [color=color] [type=type] [style=style] 
 *
 * The color option specifies the color for the labels, tic-marks,
 * and borders of the chart.  Use one of the 16 standard GRASS color
 * names.  The type option is either "area" or "cells," the default
 * is "cells" The style option is either "pie" or "bar," the default
 * is "bar"  
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#define USAGE	"name=mapname [color=color] [type=type] [style=style]"
#include "gis.h"
#define MAIN
#include "options.h"
#include "dhist.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
        int text_height;
        int text_width;
        char buff[256] ;
	char window_name[64] ;
	char *mapset ;
	struct Categories cats ;
	struct Range range ;
	struct Colors colors ;
	extern int stash_away() ;
        char title[512];
        int tt,tb,tl,tr;
        int t,b,l,r;

/* Initialize the GIS calls */
	G_gisinit("Dhistogram") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	if (! strlen(map_name))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	mapset = G_find_cell (map_name, "") ;
	if (mapset == NULL)
	{
	sprintf(buff,"Cellfile [%s] not available", map_name);
	G_fatal_error(buff) ;
	}

	if (G_read_colors(map_name, mapset, &colors) == -1)
	{
	sprintf(buff,"color file for [%s] not available", map_name) ;
		G_fatal_error(buff) ;
	}

	if (G_read_cats(map_name, mapset, &cats) == -1)
	{
	sprintf(buff,"Category file for [%s] not available", map_name) ;
		G_fatal_error(buff) ;
	}

	if (G_read_range(map_name, mapset, &range) == -1)
	{
	sprintf(buff,"Range information for [%s] not available", map_name) ;
		G_fatal_error(buff) ;
	}

/* get the distribution statistics */

	get_stats(map_name,&dist_stats);

/* set up the graphics driver and initialize its color-table */

	R_open_driver();
 
	if (D_get_cur_wind(window_name))
        	G_fatal_error("No current window");
 
	if (D_set_cur_wind(window_name))
      		G_fatal_error("Current window not available");
	 
	D_reset_colors(&colors);

/* draw a title for */

	sprintf(title,"%s in mapset %s",map_name,mapset);
	D_get_screen_window(&t, &b, &l, &r);
        text_height = (b-t)*0.05;
        text_width = (r-l)*0.05*0.50; 
        R_text_size(text_width,text_height);
        R_get_text_box(title,&tt,&tb,&tl,&tr);
        R_move_abs((int)(l+(r-l)/2-(tr-tl)/2),(int)(t+(b-t)*0.07));
        R_standard_color(color);
	R_text(title);

/* plot the distributrion statistics */

	if (style == PIE)
	   pie(dist_stats);
        else
           bar(dist_stats);

        R_flush();
	R_close_driver();
}
