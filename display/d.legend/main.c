#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"

int main( int argc, char **argv )
{
	char *mapset ;
	char buff[512] , tmp_buf1[50], tmp_buf2[50], *descr;
	char map_name[64] ;
	char window_name[64] ;
	int black ;
	int cats_num ;
	int color ;
	int cur_dot_row ;
	int do_cats ;
	int dots_per_line ;
	int dot_rows_per_box;
	int i, j, k ;
	int lines ;
	int new_colr, fp;
	int t, b, l, r ;
	int type ;
	int white ;
	int x_box[5] , px_box[5];
	int y_box[5] , py_box[5];
	struct Categories cats ;
	struct Colors colors ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4 ;
	struct Range range ;
	CELL min_ind, max_ind, null_cell;
	DCELL dmin, dmax, val;

	module = G_define_module();
	module->description =
		"improved version of d.legend that allows:\n"
		"- Thinning the categories to be represented in the legend\n"
		"- Displaying a continuous gradient of all categories in the legend\n"
		"- Interactive mouse placement of the smooth gradient box.";

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
			fprintf (stdout,"Don't know the color %s\n", opt2->answer) ;
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

        fp = G_raster_map_is_fp(map_name, mapset);

	if (G_read_cats(map_name, mapset, &cats) == -1)
	{
		sprintf(buff,"Category file for [%s] not available", map_name) ;
		G_fatal_error(buff) ;
	}

	G_set_c_null_value(&null_cell, 1);

	if (R_open_driver() != 0)
		G_fatal_error("No graphics device selected");

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
	if(!fp)
	{
	   if (G_read_range(map_name, mapset, &range) == -1)
	   {
	     sprintf(buff,"Range information for [%s] not available", map_name);
	     G_fatal_error(buff) ;
	   }
	   G_get_range_min_max (&range, &min_ind, &max_ind);
	   cats_num = max_ind - min_ind + 1 ;
        }
	else
	{
	   cats_num = cats.ncats ;
	   min_ind = 1;
	   max_ind = cats.ncats;
        }
	if (lines == 1) lines = cats_num + 1 ;
	do_cats = cats_num > (lines - 1) ? lines - 1 : cats_num ;

	/* Figure number of lines, number of pixles per line and text size */
	dots_per_line = (b - t) / lines ;
	R_text_size((int)(dots_per_line*4/5), (int)(dots_per_line*4/5)) ;

/* dots_per_line is num of pixels in each cat descr vertically */
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

	/* Set up box arrays */
	px_box[0] = 0                 ;
	py_box[0] = 0                 ;
	px_box[1] = 0                 ;
	py_box[1] = -1                ;
	px_box[2] = (dots_per_line-6) ;
	py_box[2] = 0                 ;
	px_box[3] = 0                 ;
	py_box[3] = 1                 ;
	px_box[4] = (6-dots_per_line) ;
	py_box[4] = 0                 ;
	/* Draw away */
	cur_dot_row = t + dots_per_line/2;
	j = do_cats == cats_num ? 1 : 2 ;
	dot_rows_per_box = dots_per_line - 6;
	for(i=min_ind-1; j<=do_cats && i<=max_ind; j++, i++)
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
		if(i<min_ind) /* no data cell */
		{
		   D_color(null_cell,&colors) ;
		   R_move_abs(l+4, (cur_dot_row-3)) ;
		   R_polygon_rel(x_box, y_box, 5) ;
                }
                else if(!fp)
		{
		   D_color((CELL)i,&colors) ;
		   R_move_abs(l+4, (cur_dot_row-3)) ;
		   R_polygon_rel(x_box, y_box, 5) ;
                }
                else
		{
		   descr = G_get_ith_d_raster_cat(&cats, i-1, &dmin, &dmax);
		   if(dmax==dmin)
		   /* draw a 1-color rectangle */
		   {
		      val = dmin;
		      D_d_color(dmin, &colors) ;
		      R_move_abs(l+4, (cur_dot_row-3)) ;
		      R_polygon_rel(x_box, y_box, 5) ;
                   }
		   else
		   /* split rectangle in dot_rows_per_box rows,
		      and lookup color for each box row separately */
		   {
		      val = dmin;
		      for(k=0; k<dot_rows_per_box; k++)
		      {
		         D_d_color(val,&colors) ;
		         R_move_abs(l+4, cur_dot_row-3-k) ;
		         R_polygon_rel(px_box, py_box, 5) ;
		         val = val + (dmax-dmin)/(double) dot_rows_per_box;
		      }
		   } /*fp map */
                } /* drawing box */

		/* Draw text */
		R_standard_color(color) ;
		if(!i) /* no data cell */
		   sprintf(buff, " no data");
                else if(!fp)
		   sprintf(buff, "%2d) %s", i, G_get_cat(i, &cats)) ;
                else if(dmin==dmax)
		{
		   sprintf(tmp_buf1, "%.10f", dmin);
		   G_trim_decimal(tmp_buf1);
		   sprintf(buff, "%s) %s", tmp_buf1, descr);
		}
		else
		{  
		   sprintf(tmp_buf1, "%.10f", dmin);
		   G_trim_decimal(tmp_buf1);
		   sprintf(tmp_buf2, "%.10f", dmax);
		   G_trim_decimal(tmp_buf2);
		   sprintf(buff, "%s - %s) %s", tmp_buf1, tmp_buf2, descr);
                }
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
