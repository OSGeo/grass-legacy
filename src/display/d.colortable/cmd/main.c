/*
 *   d.colortable
 *
 *   Print a colortable for a map 
 */

#include "gis.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char *map_name ;
	char *D_color_list();
	int color ;
	int lines ;
	int cols ;

	char buff[256] ;
	char window_name[64] ;
	char *mapset ;
	struct Range range ;
	struct Colors colors ;
	double ratio ;
	double sqrt() ;
	int cats_num ;
	int cur_dot_row ;
	int cur_dot_col ;
	int dots_per_line ;
	int dots_per_col ;
	int atcat ;
	int white ;
	int black ;
	int atcol ;
	int atline ;
	int i ;
	int t, b, l, r ;
	int new_colr;
	int x_box[5] ;
	int y_box[5] ;
	struct Option *opt1, *opt2, *opt3, *opt4 ;

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of existing raster file" ;

	opt2 = G_define_option() ;
	opt2->key        = "color" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = "white" ;
	opt2->options=D_color_list();
	opt2->description= "Color of lines separating the colors of the color table";

	opt3 = G_define_option() ;
	opt3->key        = "lines" ;
	opt3->type       = TYPE_INTEGER ;
	opt3->options    = "1-1000" ;
	opt3->description= "Number of lines" ;

	opt4 = G_define_option() ;
	opt4->key        = "cols" ;
	opt4->type       = TYPE_INTEGER ;
	opt4->options    = "1-1000" ;
	opt4->description= "Number of columns" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	map_name = opt1->answer;

	if (opt2->answer != NULL)
	{
		new_colr = D_translate_color(opt2->answer);
		color = new_colr;
	}

	lines = 0;
	if (opt3->answer != NULL)
		sscanf(opt3->answer,"%d",&lines);

	cols = 0;
	if (opt4->answer != NULL)
		sscanf(opt4->answer,"%d",&cols);

	/* Make sure map is available */
	mapset = G_find_cell2 (map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Cellfile [%s] not available", map_name);
		G_fatal_error(buff) ;
		exit(-1);
	}
	if (G_read_colors(map_name, mapset, &colors) == -1)
	{
		sprintf(buff,"R_color file for [%s] not available", map_name) ;
		G_fatal_error(buff) ;
		exit(-1);
	}
	if (G_read_range(map_name, mapset, &range) == -1)
	{
		sprintf(buff,"Range file for [%s] not available", map_name) ;
		G_fatal_error(buff) ;
		exit(-1);
	}
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current frame") ;
	if (D_set_cur_wind(window_name))
		G_fatal_error("Current frame not available") ;

	D_set_colors(&colors);

	/* Figure out where to put boxes */
	D_get_screen_window(&t, &b, &l, &r) ;

	cats_num = range.pmax - range.nmin + 1 ;
	if (lines <= 0 && cols <= 0)
	{
		double dx, dy ;
		dy = (double)(b-t) ;
		dx = (double)(r-l) ;
		ratio = dy / dx ;
		cols = 1 + sqrt((double)(range.pmax - range.nmin + 1) / ratio) ;
		lines = 1 + cats_num / cols ;
	}
	else if(lines > 0 && cols <= 0)
	{
		cols = 1 + cats_num / lines  ;
	}
	else if(cols > 0 && lines <= 0)
	{
		lines = 1 + cats_num / cols  ;
	}
	/* otherwise, accept without complaint what the user requests
	 * It is possible that the number of lines and cols is not
	 * sufficient for the number of categories.
	 */

	dots_per_line = (b-t) / lines ;
	dots_per_col  = (r-l) / cols  ;

	x_box[0] = 0                 ;
	y_box[0] = 0                 ;
	x_box[1] = 0                 ;
	y_box[1] = (6-dots_per_line) ;
	x_box[2] = (dots_per_col -6) ;
	y_box[2] = 0                 ;
	x_box[3] = 0                 ;
	y_box[3] = (dots_per_line-6) ;
	x_box[4] = (6-dots_per_col ) ;
	y_box[4] = 0                 ;

	atcat = range.nmin ;
	white = D_translate_color("white") ;
	black = D_translate_color("black") ;
	for(atcol=0; atcol<cols; atcol++)
	{
		cur_dot_row = t ;
		cur_dot_col = l + atcol * dots_per_col ;
		for(atline=0; atline<lines; atline++)
		{
			cur_dot_row += dots_per_line;
			/* Draw white box */
			R_standard_color(color) ;
			R_move_abs(cur_dot_col+2, (cur_dot_row-1)) ;
			R_cont_rel(0, (2-dots_per_line)) ;
			R_cont_rel((dots_per_col-2), 0) ;
			R_cont_rel(0, (dots_per_line-2)) ;
			R_cont_rel((2-dots_per_col), 0) ;
			/* Draw black box */
			R_standard_color(black) ;
			R_move_abs(cur_dot_col+3, (cur_dot_row-2)) ;
			R_cont_rel(0, (4-dots_per_line)) ;
			R_cont_rel((dots_per_col-4), 0) ;
			R_cont_rel(0, (dots_per_line-4)) ;
			R_cont_rel((4-dots_per_col), 0) ;
			/* Color box */
			D_color((CELL)atcat, &colors) ;
			R_move_abs(cur_dot_col+4, (cur_dot_row-3)) ;
			R_polygon_rel(x_box, y_box, 5) ;

			if(++atcat > range.pmax) break ;
		}
		if(atcat > range.pmax) break ;
	}

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();
}
