/*  %W%  %G%  */

/*
 *   Dlegend
 *
 *   Usage:  Dlegend mapname [color]
 *           Dlegend name=mapname color=name type=type lines=nlines
 *
 *   Print a legend for a map 
 */

#define USAGE	"name=mapname [color=name]"
#include "gis.h"
#define MAIN
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[256] ;
	char window_name[64] ;
	char *mapset ;
	struct Categories cats ;
	struct Range range ;
	struct Colors colors ;
	int white ;
	int black ;
	int cats_num ;
	int cur_dot_row ;
	int dots_per_line ;
	int dots_per_point ;
	int i ;
	int do_cats ;
	int t, b, l, r ;
	int x_box[5] ;
	int y_box[5] ;
	extern int stash_away() ;

/* Initialize the GIS calls */
	G_gisinit("Dlegend") ;

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

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	D_reset_colors(&colors);

	white = D_translate_color("white") ;
	black = D_translate_color("black") ;

/* Figure out where to put text */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;

/* How many categories to show */
	cats_num = range.pmax - range.nmin + 1 ;
	do_cats = (cats_num + 1) < lines ? cats_num : lines -1 ;

/* Figure number of lines, number of pixles per line and text size */
	if (lines == 0) lines = cats_num + 1 ;
	dots_per_line = (b - t) / lines ;
	R_text_size((int)(dots_per_line*4/5), (int)(dots_per_line*4/5)) ;

/* Set up box arrays */
	x_box[0] = 0                 ;
	y_box[0] = 0                 ;
	x_box[1] = 0                 ;
	y_box[1] = (6-dots_per_line) ;
	x_box[2] = (dots_per_line-6) ;
	y_box[2] = 0                 ;
	x_box[3] = 0                 ;
	y_box[3] = (dots_per_line-6) ;
	x_box[4] = (6-dots_per_line) ;
	y_box[4] = 0                 ;

/* Draw away */
	cur_dot_row = t + dots_per_line/2;
	for(i=range.nmax; i<=range.pmax; i++)
	{
	/* White box */
		R_standard_color(white) ;
		cur_dot_row += dots_per_line;
		R_move_abs(l+2, (cur_dot_row-1)) ;
		R_cont_rel(0, (2-dots_per_line)) ;
		R_cont_rel((dots_per_line-2), 0) ;
		R_cont_rel(0, (dots_per_line-2)) ;
		R_cont_rel((2-dots_per_line), 0) ;

	/* Black box */
		R_standard_color(black) ;
		R_move_abs(l+3, (cur_dot_row-2)) ;
		R_cont_rel(0, (4-dots_per_line)) ;
		R_cont_rel((dots_per_line-4), 0) ;
		R_cont_rel(0, (dots_per_line-4)) ;
		R_cont_rel((4-dots_per_line), 0) ;

	/* Color solid box */
		R_color(i) ;
		R_move_abs(l+4, (cur_dot_row-3)) ;
		R_polygon_rel(x_box, y_box, 5) ;

	/* Draw text */
		R_standard_color(color) ;
		sprintf(buff, "%2d) %s", i, G_get_cat(i, &cats)) ;
		R_move_abs((l+3+dots_per_line), (cur_dot_row)) ;
		R_text(buff) ;
	}

/* Add this command to list */
/*
	strcpy(buff, argv[0]) ;
	for(i=1; i<argc; i++)
	{
		strcat(buff, " ") ;
		strcat(buff, argv[i]) ;
	}
	D_add_to_list(buff) ;
*/

	R_close_driver();
}
