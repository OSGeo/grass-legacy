/*
bump? * $Id$
 *
 * 11/2002: added 'at' option, prettier fp labels, vertical flipping, 
 *            more robust mouse selection, fixed lines=x to actually
 *            display x lines, fixed mouse placement bug for non-fp,
 *            cleaned out some useless code (non-fp), re-centered color
 *            box in frame, fixed smoothed color display limits,
 *            reimplemented n flag, reduced labelnum when few cats,
 *            auto-scale text when position not explicitly set, and
 *            other cleanups.  Hamish Bowman
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
#include <math.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

/* height to width ratio when generating automatic smooth legends */
#define LEGEND_HTOW 12

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
	int do_cats ;
	int dots_per_line ;
        int thin ;
        int i, j, k;
	int lines, steps ;
	int new_colr, fp;
	int t, b, l, r ;
	int hide_catnum, hide_catstr, hide_nodata, do_smooth, use_mouse;
	char *cstr;
	int white ;
	int x_box[5];
	int y_box[5];
	struct Categories cats ;
	struct Colors colors ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt4, *opt5, *opt6, *opt7;
	struct Flag *hidestr, *hidenum, *hidenodata, *smooth, *mouse;
	struct Range range ;
	struct FPRange fprange ;
	CELL min_ind, max_ind, null_cell;
	DCELL dmin, dmax, val;
	int x0, x1, y0, y1, xyTemp;
	double X0, X1, Y0, Y1;
	int SigDigits, MaxLabelLen;
	char DispFormat[5];	/*  %.Xf\0  */
	int flip, horiz;

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
	opt6->description= "Number of text labels for smooth gradient legend" ;

	opt7 = G_define_option() ;
	opt7->key        = "at";
	opt7->key_desc   = "x1,y1,x2,y2";
	opt7->type       = TYPE_DOUBLE;
	opt7->required   = NO;
	opt7->options    = "0-100" ;
	opt7->description= "Screen coordinates to place the legend (as percentage)" ;

	hidestr = G_define_flag ();
	hidestr->key = 'v';
	hidestr->description = "Do not show category labels";

	hidenum = G_define_flag ();
	hidenum->key = 'c';
	hidenum->description = "Do not show category numbers";

        hidenodata = G_define_flag ();
	hidenodata->key = 'n';
	hidenodata->description = "Skip categories with no label";

	smooth = G_define_flag ();
	smooth->key = 's';
	smooth->description = "Draw smooth gradient";

	mouse = G_define_flag ();
	mouse->key = 'm';
	mouse->description = "Use mouse to size & place legend";

	/* Check command line */
	if (G_parser(argc, argv))
		exit(-1);

	strcpy(map_name, opt1->answer) ;

        hide_catstr = hidestr->answer;  /* note hide_catstr gets changed and re-read below */
        hide_catnum = hidenum->answer;
	hide_nodata = hidenodata->answer;
	do_smooth = smooth->answer;
	use_mouse = mouse->answer;
	
	color = 0;	/* if only to get rid of the compiler warning  */
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
	
	thin = 1;
	if (opt5->answer != NULL)
		sscanf(opt5->answer,"%d", &thin);
	if(!thin) thin=1;

	if (opt6->answer != NULL)
		sscanf(opt6->answer,"%d",&steps);

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
		flip = 1;
	}
	else flip = 0;

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

	
	if(use_mouse)
		get_legend_box(&x0, &x1, &y0, &y1);
	else {
		if (opt7->answer != NULL) {
			sscanf(opt7->answers[0], "%lf", &X0) ;
			sscanf(opt7->answers[1], "%lf", &Y0) ;
			sscanf(opt7->answers[2], "%lf", &X1) ;
			sscanf(opt7->answers[3], "%lf", &Y1) ;
			x0 = (int)(X0*(r-l)/100.);
			x1 = (int)(X1*(r-l)/100.);
			y0 = (int)(Y0*(b-t)/100.);
			y1 = (int)(Y1*(b-t)/100.);
		}
		else {	/* default */
			x0 = l+4;
			y0 = t+4;
			y1 = b-4;
			x1 = x0 + (y1 - y0)/LEGEND_HTOW;
		}
	}
	if( y0 > y1) {		/* allow for variety in order of corner */
		flip = !flip;	/*   selection without broken output    */
		xyTemp = y0;
		y0 = y1;
		y1 = xyTemp;
	}
	if( x0 > x1) {
		xyTemp = x0;
		x0 = x1;
		x1 = xyTemp;
	}
	
	horiz = (x1-x0 > y1-y0);
	if(horiz)
		fprintf(stderr, "Drawing horizontal legend as box width exceeds height.\n");
	
	if(!fp && horiz)	/* better than nothing */
		do_smooth = 1;	
	
	MaxLabelLen = 1;	/* init variable */

	/* How many categories to show */
	if(!fp) {
		if (G_read_range(map_name, mapset, &range) == -1) {
			sprintf(buff,"Range information for [%s] not available (run r.support)", map_name);
			G_fatal_error(buff) ;
		}
		G_get_range_min_max (&range, &min_ind, &max_ind);

		
		/*  cats_num is total number of categories in raster                  */
		/*  do_cats is  total number of categories to be displayed            */
		/*  k is number of cats to be displayed after skipping unlabeled cats */
		/*  lines is number of text lines/legend window                       */
 
		cats_num = max_ind - min_ind + 1 ;

		if (lines == 0) lines = cats_num ;

		do_cats = cats_num > lines ? lines : cats_num ;

		if(do_cats == cats_num)
			lines = (int)ceil((1.0*lines)/thin);
	
		/* see how many boxes there REALLY will be */
		j = 1;
		k = 0;
		for(i=min_ind; j<=do_cats && i<=max_ind; j++, i+=thin) {
		    if(!flip)
			cstr = G_get_cat(i, &cats);
		    else
			cstr = G_get_cat((max_ind - (i-min_ind)), &cats);

		    if(!cstr[0]) {  /* no cat label found, skip str output */
			if(hide_nodata)
			    continue;
		    }
		    else {
			if( !hide_catstr && (MaxLabelLen < strlen(cstr)) )
			    MaxLabelLen=strlen(cstr);
		    }
			    
		    k++;    /* count of actual boxes drawn (hide_nodata option invaidates using j-1) */
		}
		lines = k;

		if(MaxLabelLen > 1) {		/* ie we've picked up at least one label */
			MaxLabelLen++;			/* compensate for leading space */
			if(!hide_catnum)
				MaxLabelLen +=4;	/* compensate for "%2d) " */
			if(lines > 99)			/* compensate for "100) " */
				MaxLabelLen += (int)(floor(log10(lines))-1);
		}
		
		/* following covers both the above if(do_cats == cats_num) and k++ loop */
		if(lines < 1) {
			lines = 1;	/* ward off the dpl floating point exception */
			sprintf(buff,"Nothing to draw! (no categories with labels?)");
			G_fatal_error(buff) ;
		}
			
		/* Figure number of lines, number of pixles per line and text size */
		dots_per_line = ((b - t) / lines);
	
		if ((dots_per_line == 0) && (do_smooth == 0)) {
			do_smooth = 1; /* for CELL maps with lot's of cats */
			fprintf(stderr, "Forcing smooth legend as too many categories for current monitor height.\n");
		}	/* an alternate solution is to set   dots_per_line=1   */

		/* center really tiny legends */
		if( !use_mouse && opt7->answer == NULL)	{	/* if defualt scaling */
			if( !do_smooth && (dots_per_line < 4) )	/* if so small that there's no box */
				if( (b-(dots_per_line*lines))/(b*1.0) > 0.15)	/* if there's more than a 15% blank at the bottom */
					y0 = ((b-t) - (dots_per_line*lines))/2;
		}

	/*	R_text_size((int)(dots_per_line*4/5), (int)(dots_per_line*4/5)) ;    redundand */
		/* if(G_is_c_null_value(&min_ind) && G_is_c_null_value(&max_ind))
		   {
		    min_ind = 1;
		    max_ind = 0;
		   } */
        }
	else { /* is fp */
		if (G_read_fp_range(map_name, mapset, &fprange) == -1) {
			sprintf(buff,"Range information for [%s] not available", map_name);
			G_fatal_error(buff) ;
		}
		G_get_fp_range_min_max(&fprange, &dmin, &dmax);
		do_cats = 0;	/* if only to get rid of the compiler warning  */
		cats_num = 0;	/* if only to get rid of the compiler warning  */
	}
	


	
	if(do_smooth){
	    int wleg, lleg, dx, dy;
	    int txsiz;
	    int ppl;
	    int tcell;

	    if(horiz){
		lleg = x1-x0;
		dx = 0;
		dy = y1-y0;
		if(fp)
		    flip = !flip;	/* horiz floats look better not flipped by default */
	    }
	    else{
		lleg = y1-y0;
		dy = 0;
		dx = x1-x0;
	    }

	    if(!fp) {				/* cut down labelnum so they don't repeat */
		if(do_cats < steps)
		    steps = do_cats;
		if(1 == steps) steps = 2;	/* ward off the ppl floating point exception */ 
	    }

	    R_move_abs(x0, y0);
	    txsiz = (int)((y1-y0)/20);
	    ppl = (lleg)/(steps-1);
	    R_text_size(txsiz, txsiz);

	    for (k = 0; k < lleg; k++){
		if (!fp){
			if(!flip)
				tcell = min_ind + k * (double)(1 + max_ind - min_ind)/lleg;
			else
				tcell = (max_ind+1) - k * (double)(1 + max_ind - min_ind)/lleg;
			D_color((CELL)tcell,&colors) ;
		}
		else {
			if(!flip)
				val = dmin + k * (dmax - dmin)/lleg;
			else
				val = dmax - k * (dmax - dmin)/lleg;
			D_d_color(val,&colors) ;
		}
				
		R_cont_rel(dx,dy) ;
		R_move_rel(dx? -dx:1,dy? -dy:1) ;
	    }

	    if(!horiz){
	/*  if(!use_mouse || !horiz){    ???? what was this || all about */
		R_standard_color(color) ;
		for(k = 0; k< steps; k++){
		    /* Draw text */
		    if (!fp){	
			if(!flip)
			    tcell = min_ind + k * (double)(max_ind - min_ind)/(steps-1);
			else
			    tcell = max_ind - k * (double)(max_ind - min_ind)/(steps-1);

		        cstr = G_get_cat(tcell, &cats);
		        if(!cstr[0]) /* no cats found, disable str output */
			    hide_catstr=1;
			else
			    hide_catstr = hidestr->answer;
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
			if(!flip)
				val = dmin + k * (dmax - dmin)/(steps-1);
			else
				val = dmax - k * (dmax - dmin)/(steps-1); 

			if( 0 == (dmax - dmin) )		/* trap divide by 0 for single value rasters */
				sprintf(buff, "%f", val);
			else {
				SigDigits = (int)ceil(log10(fabs(25/(dmax - dmin)))); /* determine how many significant digits to display based on range */
				if(SigDigits < 0)
					SigDigits = 0;
				if(SigDigits < 7)
					sprintf(DispFormat, "%%.%df", SigDigits);
				else
					sprintf(DispFormat, "%%.2g");	/* eg 4.2e-9  */
				sprintf(buff, DispFormat, val);
			}
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
	else{   /* non FP, no smoothing */

		int txsiz, true_r;
		float ScaleFactor = 1.0;
		
		/* set legend box bounds */
		true_r = r;	/* preserve window width */
		t=y0;
		l=x0;
		b=y1;
		r=x1;   

		R_move_abs(x0, y0);

		/* figure out box height  */
		if(do_cats == cats_num)
			dots_per_line = (b - t) / (lines+1);	/* +1 line for the two 1/2s at top and bottom */
		else
			dots_per_line = (b - t) / (lines+2) ;	/* + another line for 'x of y categories' text */

		/* adjust text size */
	/*	txsiz = (int)((y1-y0)/(1.5*(lines+5)));	*/
		txsiz = (int)((y1-y0)/(2.0*lines));

		/* scale text to fit in window if position not manually set */
		if( !use_mouse && opt7->answer == NULL)	{	/* ie defualt scaling */
			ScaleFactor = (true_r/((MaxLabelLen+3)*txsiz*0.81));	/* ?? txsiz*.81=actual text width. */
			if( ScaleFactor < 1.0) {
				txsiz = (int)floor(txsiz*ScaleFactor);
				dots_per_line = (int)floor(dots_per_line*ScaleFactor);
			}
		}

		if(dots_per_line < txsiz)
			txsiz= dots_per_line;

		R_text_size(txsiz, txsiz);	
		
	
		/* Set up box arrays */
		x_box[0] = 0                 ;
		y_box[0] = 0                 ;
		x_box[1] = 0                 ;
		y_box[1] = (6-dots_per_line) ;
		x_box[2] = (dots_per_line-5) ; /* was dpl-6. Does this (-5) cause incorrect output on non i386 ??? */
		y_box[2] = 0                 ;
		x_box[3] = 0                 ;
		y_box[3] = (dots_per_line-5) ; /* was dpl-6. Funny feeling that it might. (old bug log?) */
		x_box[4] = (6-dots_per_line) ;
		y_box[4] = 0                 ;


		/* Draw away */

/*		if(ScaleFactor < 1.0)   */
/*		    cur_dot_row = ((b-t) - (dots_per_line*lines))/2; */ /* this will center the legend */
/*		else	*/
		 cur_dot_row = t + dots_per_line/2;

		/*  j = (do_cats == cats_num ? 1 : 2 );  */
		j = 1;
		k = 0;
		for(i=min_ind; j<=do_cats && i<=max_ind; j++, i+=thin)
		{

		    if(!flip)
			cstr = G_get_cat(i, &cats);
	            else
			cstr = G_get_cat((max_ind - (i-min_ind)), &cats);
		    
		    if(!cstr[0]) {  /* no cat label found, skip str output */
			hide_catstr=1;
			if(hide_nodata)
			    continue;
		    }
		    else
			hide_catstr = hidestr->answer;
		    
		    k++;	/* count of actual boxes drawn (hide_nodata option invaidates using j-1) */
			
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
		    if(!flip)
			D_color((CELL)i,&colors) ;
		    else
			D_color((CELL)(max_ind - (i-min_ind)),&colors) ; 
		    		    
		    R_move_abs(l+4, (cur_dot_row-3)) ;
		    R_polygon_rel(x_box, y_box, 5) ;

		    /* Draw text */
		    R_standard_color(color) ;

	            if(hide_catnum && ! hide_catstr) /* str only */
			sprintf(buff, " %s", cstr);
	            else{
		        if(! hide_catnum && hide_catstr) { /* num only */
			    if(!flip)			
				sprintf(buff, "%2d", i);
		            else
				sprintf(buff, "%2d", max_ind - (i-min_ind));
			}
			else{
		            if(hide_catnum && hide_catstr) /* nothing, box only */
		               buff[0] = 0;
		            else {
				if(!flip)
				    sprintf(buff, "%2d) %s", i, cstr); /* both */
				else
				    sprintf(buff, "%2d) %s", (max_ind - (i-min_ind)), cstr);
			    }
			}
		    }
		    R_move_abs((l+3+dots_per_line), (cur_dot_row)-3);
		    R_text(buff);
		}
		if (do_cats != cats_num)
		{
		    cur_dot_row += dots_per_line;
	/*	    sprintf(buff, "%d of %d categories\n", (j-1), cats_num) ;	*/

		    sprintf(buff, "%d of %d categories\n", k, cats_num) ;
		
		    /* shrink text if it will run off the screen */
		    MaxLabelLen=strlen(buff)+4;
		    ScaleFactor = ((true_r-l)/(MaxLabelLen*txsiz*0.81));	/* ?? txsiz*.81=actual text width. */
		    if( ScaleFactor < 1.0) {
			txsiz = (int)floor(txsiz*ScaleFactor);
			R_text_size(txsiz, txsiz);
		}
		    R_standard_color(white) ;
		    R_move_abs((l+3+dots_per_line), (cur_dot_row)) ;
		    R_text(buff) ;
		}
	}
	if(!use_mouse)
	    D_add_to_list(G_recreate_command()) ;

	R_close_driver();
	exit(0);
}
