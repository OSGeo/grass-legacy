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

	/* kang - added */
	int cat;
	int cr, cg, cb ;
	struct Categories cats;
	CELL min, max, n;
	char *label;


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

/* kang - add all the followings */

	/* find min and max cat */
	G_get_range_min_max (&range, &min, &max);
	G_read_cats (map_name, mapset, &cats);

	/* free category after done - ? */

	/* get the color in */
	G_get_color (cat, &cr, &cg, &cb, &colors) ;
	/* free it when done */

	for (n=min; n <= max; n++) {
		G_squeeze(label = G_get_cat (n, &cats));
		G_get_color (n, &cr, &cg, &cb, &colors);
		printf ("#%02x%02x%02x %d %s\n", cr, cg, cb, n, label);
	}
}
