/*
 *  8/2002: added o flag MN
 * 11/2001: added c,v flags, fixed mouse for CELL maps MN
 * 10/2001: added labelnum MN
 *
 * This file is a first attempt to merge the 5.0 version of d.leg.thin
 * and the 4.3 code which supports the flags.
 *
 * The old 4.x code is here:
 *  src421/untested/display/d.leg.thin/
 *
 */

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

/* height to width ratio when generating automatic smooth legends */
#define LEGEND_HTOW 8

#define VAL     0x1
#define CAT     0x10

int main( int argc, char **argv )
{
	char *mapset ;
	char buff[512];
	char map_name[64] ;
	char window_name[64] ;
	int black ;
	int cats_num ;
	int color ;
	int cur_dot_row ;
	int do_cats = 0;
	int dots_per_line ;
        int thin ;
        int i, j;
	int lines, steps ;
	int new_colr, fp;
	int t, b, l, r ;
	int hide_catnum, hide_catstr, do_smooth, use_mouse;
	char *cstr;
	int white ;
	int x_box[5];
	int y_box[5];
	struct Categories cats ;
	struct Colors colors ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt4, *opt5, *opt6, *opt7;
	struct Flag *hidestr, *hidenum, *smooth, *catsonly, *mouse;
	struct Range range ;
	struct FPRange fprange ;
	CELL min_ind, max_ind, null_cell;
	DCELL dmin, dmax, val;
	int x0, x1, y0, y1;
	int plaincats_num, maxfontsize;
	struct Histogram histogram;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Displays  a  legend  for a raster map layer in the active "
		"frame on the graphics monitor.";

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
	opt5->key        = "thin" ;
	opt5->type       = TYPE_INTEGER ;
	opt5->required   = NO;
	opt5->answer     = "1" ;
	opt5->options    = "1-1000" ;
	opt5->description= "Thinning factor (thin=10 gives cats 0,10,20...)";

	opt6 = G_define_option() ;
	opt6->key        = "labelnum" ;
	opt6->type       = TYPE_INTEGER ;
	opt6->answer     = "5" ;
	opt6->options    = "2-100" ;
	opt6->description= "Number of labels for legend description" ;

	opt7 = G_define_option() ;
	opt7->key        = "maxfontsize" ;
	opt7->type       = TYPE_INTEGER ;
	opt7->required   = NO;
	opt7->answer     = "20" ;
	opt7->options    = "0-1000" ;
	opt7->description= "Max font size for text labels";


	hidestr = G_define_flag ();
	hidestr->key = 'v';
	hidestr->description = "Do not show category labels";

	hidenum = G_define_flag ();
	hidenum->key = 'c';
	hidenum->description = "Do not show category numbers";

        /*	hidenodata = G_define_flag ();
	hidenodata->key = 'n';
	hidenodata->description = "Do not show no data values"; */

	smooth = G_define_flag ();
	smooth->key = 's';
	smooth->description = "Draw smooth gradient";

	catsonly = G_define_flag ();
	catsonly->key = 'o';
	catsonly->description = "Draw entries only for existing categories";

	mouse = G_define_flag ();
	mouse->key = 'm';
	mouse->description = "Use mouse to size & place legend";

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer) ;

        hide_catstr = hidestr->answer;
        hide_catnum = hidenum->answer;
        /*        hide_nodata = hidenodata->answer; */
	do_smooth = smooth->answer;
	use_mouse = mouse->answer;
	
	if (opt2->answer != NULL)
	{
		new_colr = D_translate_color(opt2->answer) ;
		if (new_colr == 0)
			G_fatal_error ("Don't know the color %s", opt2->answer) ;
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

	thin = 1;
	if (opt5->answer != NULL)
		sscanf(opt5->answer,"%d", &thin);
	if(!thin) thin=1;

	if (opt6->answer != NULL)
		sscanf(opt6->answer,"%d",&steps);

	if (opt7->answer != NULL)
		sscanf(opt7->answer,"%d",&maxfontsize);


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
	if (fp)
	{ 
	 do_smooth = TRUE;
	 fprintf(stderr, "FP map found - switching gradient legend on\n");
	}

	if (G_read_cats(map_name, mapset, &cats) == -1)
	{
		sprintf(buff,"Category file for [%s] not available", map_name) ;
		G_warning(buff) ;
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
	     sprintf(buff,"Range information for [%s] not available (run r.support)", map_name);
	     G_fatal_error(buff) ;
	   }
	   G_get_range_min_max (&range, &min_ind, &max_ind);
	/*   if(G_is_c_null_value(&min_ind) && G_is_c_null_value(&max_ind))
	   {
	    min_ind = 1;
	    max_ind = 0;
	   } */
	   do_cats = max_ind - min_ind + 1 ;
           
           if(catsonly->answer) /* print only existing categories in a thematic map */
           {
              if (G_read_histogram (map_name, mapset, &histogram) <= 0)
                  G_fatal_error("Run r.support on file <%s> in mapset <%s>",map_name,mapset);

	      plaincats_num= G_get_histogram_num (&histogram) - 1;
	      do_cats = plaincats_num + 1;
	    }
        }
	else /* is fp */
	{
	   if (G_read_fp_range(map_name, mapset, &fprange) == -1)
	   {
	     sprintf(buff,"Range information for [%s] not available", map_name);
	     G_fatal_error(buff) ;
	   }
           G_get_fp_range_min_max(&fprange, &dmin, &dmax);
        }

	if(!catsonly->answer && !fp) /* print only existing categories in a thematic map */
	   cats_num = max_ind - min_ind + 1 ;
	else
	   cats_num = do_cats ;
	
	if (lines == 1) lines = cats_num + 1 ;
	do_cats = cats_num > (lines - 1) ? lines - 1 : cats_num ;

	/* Figure number of lines, number of pixles per line and text size */
        dots_per_line = (b - t) / lines;
        if (dots_per_line > maxfontsize) 
           dots_per_line = maxfontsize;  /* avoid BIG characters in legend */
	if ((dots_per_line == 0) && (do_smooth == 0))
	{
	   do_smooth = 1; /* for CELL maps with lot's of cats */
	   fprintf(stderr, "Forcing smooth legend as too many categories for current monitor height.\n");
	   /* an alternate solution is to set   dots_per_line=1   */
	}
		
	R_text_size((int)(dots_per_line*4/5), (int)(dots_per_line*4/5)) ;

/* Here the 4.3 version starts:  */

	if(use_mouse)
		get_legend_box(&x0, &x1, &y0, &y1);
	else{
		x0 = l+4;
		y0 = t+4;
		y1 = b-4;
		x1 = x0 + (y1 - y0)/LEGEND_HTOW;
	}

	if(do_smooth){
	   int k, wleg, lleg, dx, dy, horiz;
	   int txsiz;
	   int ppl;
	   int tcell;
	    
	    horiz = (x1-x0 > y1-y0);

	    if(horiz){
		lleg = x1-x0;
		dx = 0;
		dy = y1-y0;
	    }
	    else{
		lleg = y1-y0;
		dy = 0;
		dx = x1-x0;
	    }
	    R_move_abs(x0, y0);
	    txsiz = (int)((y1-y0)/20);
	    ppl = (lleg)/(steps-1);
	    R_text_size(txsiz, txsiz);
	    for (k = 0; k < lleg; k++){
		if (!fp){
		    tcell = min_ind + k * (double)(.5 + max_ind - min_ind)/lleg;
		    D_color((CELL)tcell,&colors) ;
		}
		else {
		    val = dmin + k * (dmax - dmin)/lleg;
		    D_d_color(val,&colors) ;
		}
		
		R_cont_rel(dx,dy) ;
		R_move_rel(dx? -dx:1,dy? -dy:1) ;
	    }

	    if(!use_mouse || !horiz){
		R_standard_color(color) ;
		for(k = 0; k< steps; k++){
		    /* Draw text */
		    if (!fp){	
		        tcell = min_ind + k * (double)(max_ind - min_ind)/(steps-1);

		        cstr = G_get_cat(tcell, &cats);
		        if(!cstr[0]) /* no cats found, disable str output */
		          hide_catstr=1;

		        if(hide_catnum && ! hide_catstr) /* str only */
			    sprintf(buff, " %s", cstr);
		        else{
			    if(! hide_catnum && hide_catstr) /* num only */
			        sprintf(buff, "%2d", tcell);
			    else{
			        if(hide_catnum && hide_catstr) /* nothing, box only */
			           buff[0] = 0;
			        else
			           sprintf(buff, "%2d) %s", tcell, cstr); /* both */
			        }
			    }
		    } 
		    else {
		        /* FP map */
		        val = dmin + k * (dmax - dmin)/(steps-1);
			sprintf(buff, "%f", val);
		    }
		    if(!k) /* first  */
			R_move_abs(x1+4, y0+txsiz) ;
		    else if(k == steps-1) /* last */
			R_move_abs(x1+4,y1) ;
		    else
			R_move_abs(x1+4,y0+ppl*k + txsiz/2) ;
		    R_text(buff) ;
		} /* for */
	    } /* !use_mouse || !horiz */

	    lleg = y1-y0;
	    wleg = x1-x0;

	    /* Black box */
	    R_standard_color(black) ;
	    R_move_abs(x0+1, y0+1) ;
	    R_cont_rel(0,lleg-2) ;
	    R_cont_rel(wleg-2, 0) ;
	    R_cont_rel(0, 2-lleg) ;
	    R_cont_rel(2-wleg, 0) ;

	    /* White box */
	    R_standard_color(white) ;
	    R_move_abs(x0, y0) ;
	    R_cont_rel(0,lleg) ;
	    R_cont_rel(wleg, 0) ;
	    R_cont_rel(0, -lleg) ;
	    R_cont_rel(-wleg, 0) ;

	}
	else{   /* no smooth */

	    /* if mouse, adjust legend box to mouse settings */
	    if (mouse->answer)
	    {
	      int lleg, dx, dy, horiz, txsiz;

	      /* set legend box according to mouse settings */
	      l=x0;
	      t=y0;
	      r=x1;
	      b=y1;

	      horiz = (x1-x0 > y1-y0);
	      if(horiz){
		lleg = x1-x0;
		dx = 0;
		dy = y1-y0;
	      }
	      else{
		lleg = y1-y0;
		dy = 0;
		dx = x1-x0;
	      }
	      R_move_abs(x0, y0);
	      
	      /* adjust text size */
	      txsiz = (int)((y1-y0)/20);
	      R_text_size(txsiz, txsiz);

	      /* smaller boxes */
	      dots_per_line = (int) ((b - t) * ((float) txsiz / (float)cats_num)) ;
	    } /* mouse */
	    
	    /* Set up box arrays */
	    x_box[0] = 0                 ;
	    y_box[0] = 0                 ;
	    x_box[1] = 0                 ;
	    y_box[1] = (5-dots_per_line) ;
	    x_box[2] = (dots_per_line-5) ;
	    y_box[2] = 0                 ;
	    x_box[3] = 0                 ;
	    y_box[3] = (dots_per_line-5) ;
	    x_box[4] = (5-dots_per_line) ;
	    y_box[4] = 0                 ;


	    /* Draw away */
	    cur_dot_row = t + dots_per_line/2;
	    j = (do_cats == cats_num ? 1 : 2 );

	    if(catsonly->answer && !fp) /* print only existing categories in a thematic map */
	      min_ind = G_get_histogram_cat(0,&histogram);

	    for(i=min_ind; j<=do_cats && i<=max_ind; j++, i+=thin)
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
		    R_move_abs(l+4, (cur_dot_row-2)) ;
		    R_polygon_rel(x_box, y_box, 5) ;

		    /* Draw text */
		    R_standard_color(color) ;
		    cstr = G_get_cat(i, &cats);
	            if(!cstr[0])    /* no cats found, disable str output */
	              hide_catstr=1;
		   
	            if(hide_catnum && ! hide_catstr) /* str only */
		        sprintf(buff, " %s", cstr);
	            else{
		        if(! hide_catnum && hide_catstr) /* num only */
		            sprintf(buff, "%2d", i);
		        else{
		            if(hide_catnum && hide_catstr) /* nothing, box only */
		               buff[0] = 0;
		            else
		               sprintf(buff, "%2d) %s", i, cstr); /* both */
		            }
		        }

		    R_move_abs((l+3+dots_per_line), (cur_dot_row));
		    R_text(buff);
		    if(catsonly->answer  && !fp) /* print only existing categories in a thematic map */
		      i = G_get_histogram_cat(j,&histogram) - 1;

	    }
	    if (do_cats != cats_num)
	    {
		    cur_dot_row += dots_per_line;
		    sprintf(buff, "%d of %d categories\n", (j-2), cats_num) ;
		    R_standard_color(white) ;
		    R_move_abs((l+3+dots_per_line), (cur_dot_row)) ;
		    R_text(buff) ;
	    }
	}

	if (! mouse->answer)
	    D_add_to_list(G_recreate_command()) ;

	R_close_driver();
	exit(0);
}
