#include "gis.h"

main(argc, argv)
int argc ;
char **argv ;
{
	char *mapset ;
	char buff[512] ;
	char map_name[64] ;
	char window_name[64] ;
	char *D_color_list();
	int black ;
	int cats_num ;
	int color ;
	int cur_dot_row ;
	int do_cats ;
	int dots_per_line ;
	int i, j ;
	int lines ;
	int new_colr;
	int t, b, l, r ;
	int type ;
	int white ;
	int x_box[5] ;
	int y_box[5] ;
	struct Categories cats ;
	struct Colors colors ;
	struct Option *opt1, *opt2, *opt3, *opt4 ;
	struct Range range ;
	CELL min, max;

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of raster map" ;

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options    = D_color_list();
	opt2->description= "Sets the legend's text color" ;

/*
	opt3 = G_define_option() ;
	opt3->key        = "type" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO;
	opt3->options    = "fancy" ;
	opt3->description= "Set type is fancy or not" ;
*/

	opt4 = G_define_option() ;
	opt4->key        = "lines" ;
	opt4->type       = TYPE_INTEGER ;
	opt4->answer     = "0" ;
	opt4->options    = "0-1000" ;
	opt4->description= "Number of text lines (useful for truncating long legends)" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer) ;

	if (opt2->answer != NULL)
	{
		new_colr = D_translate_color(opt2->answer) ;
		if (new_colr == 0)
		{
			printf("Don't know the color %s\n", opt2->answer) ;
			exit(-1);
		}
		color = new_colr ;
	}

/*
	if (opt3->answer != NULL)
	{
			if (strcmp(opt3->answer,"fancy"))
					exit(-1);
			type = FANCY ;
	}
*/

	if (opt4->answer != NULL)
		sscanf(opt4->answer,"%d",&lines);
	
	lines ++ ;

	/* Make sure map is available */
	mapset = G_find_cell (map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Raster file [%s] not available", map_name);
		G_fatal_error(buff) ;
	}

	if (G_read_colors(map_name, mapset, &colors) == -1)
	{
		sprintf(buff,"Color file for [%s] not available", map_name) ;
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
	G_get_range_min_max (&range, &min, &max);

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	D_set_colors(&colors);

	white = D_translate_color("white") ;
	black = D_translate_color("black") ;

	/* Figure out where to put text */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;

	/* How many categories to show */
	cats_num = max - min + 1 ;

	if (lines == 1) lines = cats_num + 1 ;
	do_cats = cats_num > (lines - 1) ? lines - 1 : cats_num ;

	/* Figure number of lines, number of pixles per line and text size */
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
	j = do_cats == cats_num ? 1 : 2 ;
	for(i=min; j<=do_cats && i<=max; j++, i++)
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
		D_color((CELL)i,&colors) ;
		R_move_abs(l+4, (cur_dot_row-3)) ;
		R_polygon_rel(x_box, y_box, 5) ;

		/* Draw text */
		R_standard_color(color) ;
		sprintf(buff, "%2d) %s", i, G_get_cat(i, &cats)) ;
		R_move_abs((l+3+dots_per_line), (cur_dot_row)) ;
		R_text(buff) ;
	}
	if (do_cats != cats_num)
	{
		cur_dot_row += dots_per_line;
		sprintf(buff, "%d of %d categories\n", j-2, cats_num) ;
		R_standard_color(white) ;
		R_move_abs((l+3+dots_per_line), (cur_dot_row)) ;
		R_text(buff) ;
	}

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();
	exit(0);
}
