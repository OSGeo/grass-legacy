#include <string.h>
#include "display.h"
#include "D.h"
#include "raster.h"
#include "gis.h"

#define	VAL	0x1
#define	CAT	0x10

/*********
  $Id$
 
  bugfix on shifted number 2/2000 M. Neteler
  
  added "no data" flag 11/99 M. Neteler 
**********/

int main(int argc,char **argv)
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
	int y_dots_per_line, x_dots_per_line ;
	int dot_rows_per_box;
	int i, j, k ;
	int lines, color_ramp ;
	int new_colr, fp;
	int t, b, l, r ;
	int type ;
	int white ;
	int x_box[5] , px_box[5];
	int y_box[5] , py_box[5];
	int show ;
	struct Categories cats ;
	struct Colors colors ;
	struct Option *opt1, *opt2, *opt3, *opt4, *opt5 ;
	struct Range range ;
	struct Flag *flag1;
	struct FPRange fp_range ;
	CELL min_ind, max_ind, null_cell;
	DCELL dmin, dmax, val;

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

	opt5 = G_define_option() ;
	opt5->key        = "show" ;
	opt5->type       = TYPE_STRING ;
	opt5->multiple   = YES ;
	opt5->answer     = "val,cat" ;
	opt5->options    = "val,cat" ;
	opt5->description= "Show either values or categories. Or both" ;

        flag1 = G_define_flag();
        flag1->key      = 'n';
        flag1->description= "Do not display no data (NULL) values.";
                        

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
	
	show = 0;
	for(i=0; opt5->answers[i]; i++)
	{
		if(!strncmp(opt5->answers[i],"val",3))
			show |= VAL;
		else
		if(!strncmp(opt5->answers[i],"cat",3))
			show |= CAT;
	}

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
		G_warning(buff);
		color_ramp = 1;
	}

        color_ramp = 0;
	G_set_c_null_value(&null_cell, 1);

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
	if(!fp)
	{
	   if (G_read_range(map_name, mapset, &range) == -1)
	   {
	     sprintf(buff,"Range information for [%s] not available", map_name);
	     G_fatal_error(buff) ;
	   }
	   G_get_range_min_max (&range, &min_ind, &max_ind);
	   if(G_is_c_null_value(&min_ind) && G_is_c_null_value(&max_ind))
	   {
	     min_ind = 1;
	     max_ind = 0;
	   }
	   cats_num = max_ind - min_ind + 1 ;
	   /*min_ind++;*/
	   /*max_ind++;*/
	   /* to allow for null */
        }
	else /* is fp */
	{
	   if(!color_ramp)
		cats_num = G_number_of_raster_cats(&cats) ;
           else
		cats_num = 0;
	   if(cats_num<=0)
	   {
	      color_ramp = 1;
	      cats_num = 1;
           }
	   min_ind = 1;
	   max_ind = cats_num;
        }
	if (lines == 1)
	{
	    if(color_ramp) lines=15; /* just to make it long enough Olga */
	    else lines = cats_num + 1 ;
        }

	do_cats = cats_num > (lines - 1) ? lines - 1 : cats_num ;

	/* Figure number of lines, number of pixles per line and text size */
	y_dots_per_line = x_dots_per_line = (b - t) / (lines+1) ;
	R_text_size((int)(x_dots_per_line*4/5), (int)(x_dots_per_line*4/5)) ;

/* dots_per_line is num of pixels in each cat descr vertically */
	/* Set up box arrays */
	x_box[0] = 0                 ;
	y_box[0] = 0                 ;
	x_box[1] = 0                 ;
	y_box[1] = (6-y_dots_per_line) ;
	x_box[2] = (x_dots_per_line-6) ;
	y_box[2] = 0                 ;
	x_box[3] = 0                 ;
	y_box[3] = (y_dots_per_line-6) ;
	x_box[4] = (6-x_dots_per_line) ;
	y_box[4] = 0                 ;

	/* Set up box arrays */
	px_box[0] = 0                 ;
	py_box[0] = 0                 ;
	px_box[1] = 0                 ;
	py_box[1] = -1                ;
	px_box[2] = (y_dots_per_line-6) ;
	py_box[2] = 0                 ;
	px_box[3] = 0                 ;
	py_box[3] = 1                 ;
	px_box[4] = (6-x_dots_per_line) ;
	py_box[4] = 0                 ;
	/* Draw away */
	cur_dot_row = t + y_dots_per_line/2;
	if(!color_ramp)
	   j = do_cats == cats_num ? 0 : 1 ;
        else
	   j = 0;
	dot_rows_per_box = y_dots_per_line - 6;
	for(i=min_ind-1; j<=do_cats && i<=max_ind; j++, i++)
	{
#ifdef COLOR_RAMP
		/* if color ramp, draw one tall box */
		if(i==max_ind && color_ramp)
		{
		    y_dots_per_line = b - t - 2*x_dots_per_line; 
	            dot_rows_per_box = y_dots_per_line - 6;
		    /* reset the box */
	            x_box[0] = 0                 ;
	            y_box[0] = 0                 ;
	            x_box[1] = 0                 ;
	            y_box[1] = (6-y_dots_per_line) ;
	            x_box[2] = (x_dots_per_line-6) ;
	            y_box[2] = 0                 ;
	            x_box[3] = 0                 ;
	            y_box[3] = (y_dots_per_line-6) ;
	            x_box[4] = (6-x_dots_per_line) ;
	            y_box[4] = 0                 ;

	            /* Set up box arrays */
	            px_box[0] = 0                 ;
	            py_box[0] = 0                 ;
	            px_box[1] = 0                 ;
	            py_box[1] = -1                ;
	            px_box[2] = (x_dots_per_line-6) ;
	            py_box[2] = 0                 ;
	            px_box[3] = 0                 ;
            	    py_box[3] = 1                 ;
	            px_box[4] = (6-y_dots_per_line) ;
                }
#endif
		cur_dot_row += y_dots_per_line;

		/* White box */
		if (i==min_ind-1) /* check for null cat */
		{
		  if (!flag1->answer) /* do not draw when flag*/
		  {
		    R_standard_color(white) ;
		    R_move_abs(l+2, (cur_dot_row-1)) ;
		    R_cont_rel(0, (2-y_dots_per_line)) ;
		    R_cont_rel((x_dots_per_line-2), 0) ;
		    R_cont_rel(0, (y_dots_per_line-2)) ;
		    R_cont_rel((2-x_dots_per_line), 0) ;
		  }
		}
		else
		  {
		    R_standard_color(white) ;
		    R_move_abs(l+2, (cur_dot_row-1)) ;
		    R_cont_rel(0, (2-y_dots_per_line)) ;
		    R_cont_rel((x_dots_per_line-2), 0) ;
		    R_cont_rel(0, (y_dots_per_line-2)) ;
		    R_cont_rel((2-x_dots_per_line), 0) ;
		  }
		
		if ((i==min_ind-1 && !flag1->answer) || i!=min_ind-1)
		  {
		    /* Black box */
		    R_standard_color(black) ;
		    R_move_abs(l+3, (cur_dot_row-2)) ;
		    R_cont_rel(0, (4-y_dots_per_line)) ;
		    R_cont_rel((x_dots_per_line-4), 0) ;
		    R_cont_rel(0, (y_dots_per_line-4)) ;
		    R_cont_rel((4-x_dots_per_line), 0) ;
		  }


		/* Color solid box */
		if(i==min_ind-1) /* no data cell */
		{
		  if (!flag1->answer)
		  {
		    D_color(null_cell,&colors) ;
		    R_move_abs(l+4, (cur_dot_row-3)) ;
		    R_polygon_rel(x_box, y_box, 5) ;
		  }
                }
                else if(!fp)
		{
		   D_color((CELL)i,&colors) ;
		   R_move_abs(l+4, (cur_dot_row-3)) ;
		   R_polygon_rel(x_box, y_box, 5) ;
                }
                else
		{
		   if(color_ramp) 
		   {
	               if (G_read_fp_range(map_name, mapset, &fp_range) == -1)
	               {
	                 sprintf(buff,"fp Range information for [%s] not available", map_name);
	                 G_fatal_error(buff) ;
	               }
	               G_get_fp_range_min_max (&fp_range, &dmin, &dmax);
		       if(G_is_d_null_value(&dmin) || G_is_d_null_value(&dmax))
			  G_fatal_error("Floating Point Data Range is empty!");
		       descr = G_store("");
                   }
		   else
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

		buff[0] = 0;
		tmp_buf1[0] = 0;
		tmp_buf2[0] = 0;
		/* Draw text */
		R_standard_color(color) ;
		if(i==min_ind-1) /* no data cell */
		{
		 if (!flag1->answer)
		 {
		   sprintf(tmp_buf2, " no data");
		 }
		}
                else if(!fp)
		{
		   if(show & VAL)
		      sprintf(tmp_buf1, " %d)", i);
		   if(show & CAT)
		   {
/*
		      if(show & VAL)
		         strcat(tmp_buf1, " ");
*/
		      sprintf(tmp_buf2, " %s", G_get_cat(i, &cats));
		   }
		}
                else if(dmin==dmax)
		{
		   if(show & VAL)
		   {
		      sprintf(buff, "%.10f", dmin);
		      G_trim_decimal(buff);
		      sprintf(tmp_buf1, " %s)", buff);
		   }
		   if(show & CAT)
		   {
/*
		      if(show & VAL)
		         strcat(tmp_buf1, " ");
*/
		      sprintf(tmp_buf2, " %s", descr);
		   }
		}
		else
		{  
		   if(show & VAL)
		   {
		      sprintf(buff, "%.10f", dmin);
		      G_trim_decimal(buff);
		      sprintf(tmp_buf2, "%.10f", dmax);
		      G_trim_decimal(tmp_buf2);
		      sprintf(tmp_buf1, " %s - %s)", buff, tmp_buf2);
		   }
		   tmp_buf2[0] = 0;
		   if(show & CAT)
		   {
/*
		      if(show & VAL)
			 strcat(tmp_buf1, " ");
*/
		      sprintf(tmp_buf2, " %s", descr);
		   }
                }
		sprintf(buff, "%s%s", tmp_buf1, tmp_buf2) ;

		R_move_abs((l+3+x_dots_per_line), (cur_dot_row)) ;
		R_text(buff) ;
	}
	if (do_cats != cats_num)
	{
		cur_dot_row += y_dots_per_line;
		sprintf(buff, "%d of %d categories\n", j-2, cats_num) ;
		R_standard_color(white) ;
		R_move_abs((l+3+x_dots_per_line), (cur_dot_row)) ;
		R_text(buff) ;
	}

	D_add_to_list(G_recreate_command()) ;

	R_close_driver();
	exit(0);
}
