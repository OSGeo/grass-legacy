/* d.legend a.k.a d.leg.thin 
 * $Id$
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
	struct Option *opt1, *opt2, *opt4, *opt5, *opt6, *opt7, *opt8, *opt9;
	struct Flag *hidestr, *hidenum, *hidenodata, *smooth, *mouse, *flipit;
	struct Range range ;
	struct FPRange fprange ;
	CELL min_ind, max_ind, null_cell;
	DCELL dmin, dmax, val;
	CELL min_colr, max_colr;
	DCELL min_dcolr, max_dcolr;
	int x0, x1, y0, y1, xyTemp;
	double X0, X1, Y0, Y1;
	int SigDigits, MaxLabelLen;
	char DispFormat[5];	/*  %.Xf\0  */
	int flip, horiz, UserRange;
	double UserRangeMin, UserRangeMax, UserRangeTemp;
	double *catlist, maxCat;
	int catlistCount, use_catlist;

	
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
	opt2->answer     = DEFAULT_FG_COLOR ;
	opt2->options    = D_color_list();
	opt2->description= "Sets the legend's text color" ;

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
	opt7->key_desc   = "bottom,top,left,right";
	opt7->type       = TYPE_DOUBLE;		/* needs to be TYPE_DOUBLE to get past options check */
	opt7->required   = NO;
	opt7->options    = "0-100" ;
	opt7->description= "Screen coordinates to place the legend (as percentage)" ;
	opt7->answer     = NULL;
	
	opt8 = G_define_option() ;
	opt8->key        = "use";
	opt8->key_desc   = "catnum";
	opt8->type       = TYPE_DOUBLE;		/* string as it is fed through the parser? */
	opt8->required   = NO;
	opt8->description= "List of discrete category numbers/values for legend" ;
	opt8->multiple   = YES;

	opt9 = G_define_option() ;
	opt9->key        = "range";
	opt9->key_desc   = "min,max";
	opt9->type       = TYPE_DOUBLE;		/* should it be type_double or _string ??*/
	opt9->required   = NO;
	opt9->description= "Use a subset of the map range for the legend (min,max)" ;


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

	flipit = G_define_flag ();
	flipit->key = 'f';
	flipit->description = "Flip legend";


	/* Check command line */
	if (G_parser(argc, argv))
		exit(1);

	strcpy(map_name, opt1->answer) ;

        hide_catstr = hidestr->answer;  /* note hide_catstr gets changed and re-read below */
        hide_catnum = hidenum->answer;
	hide_nodata = hidenodata->answer;
	do_smooth = smooth->answer;
	use_mouse = mouse->answer;
	flip = flipit->answer;
	
	color = 0;	/* if only to get rid of the compiler warning  */
	if (opt2->answer != NULL)
	{
		new_colr = D_translate_color(opt2->answer) ;
		if (new_colr == 0)
			G_fatal_error ("Don't know the color %s", opt2->answer) ;
		color = new_colr ;
	}

	if (opt4->answer != NULL)
		sscanf(opt4->answer,"%d",&lines);
	
	thin = 1;
	if (opt5->answer != NULL)
		sscanf(opt5->answer,"%d", &thin);
	if(!thin) thin=1;

	if (opt6->answer != NULL)
		sscanf(opt6->answer,"%d",&steps);

	catlistCount = 0;
	if (opt8->answer != NULL) {	/* should this be answerS ? */
		use_catlist = TRUE;

		catlist= (double *) G_calloc(100+1,sizeof(double));
		for (i=0; i<100; i++)		/* fill with dummy values */
			catlist[i]=1.0*(i+1);
		catlist[i]=0;

		for(i=0; (opt8->answers[i] != NULL) && i<100; i++)
			catlist[i]=atof(opt8->answers[i]);

		catlistCount = i;
	}
	else
		use_catlist = FALSE;
	
	
	UserRange = FALSE;
	if (opt9->answer != NULL) {	/* should this be answerS ? */ 
		sscanf(opt9->answers[0], "%lf", &UserRangeMin);
		sscanf(opt9->answers[1], "%lf", &UserRangeMax);
		UserRange = TRUE;
		if (UserRangeMin > UserRangeMax) {
			UserRangeTemp = UserRangeMax;
			UserRangeMax = UserRangeMin;
			UserRangeMin = UserRangeTemp;
			flip = !flip;
		}
	}
	
	
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
	if (fp && !use_catlist)
	{
		do_smooth = TRUE;
		/* fprintf(stderr, "FP map found - switching gradient legend on\n"); */
		flip = !flip;
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

	white = D_translate_color(DEFAULT_FG_COLOR) ;
	black = D_translate_color(DEFAULT_BG_COLOR) ;

	/* Figure out where to put text */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;

	if(use_mouse) {
	    if(!get_legend_box(&x0, &x1, &y0, &y1))
		exit(0);
	}
	else {
		if (opt7->answer != NULL) {	/* should this be answerS ? */
			sscanf(opt7->answers[0], "%lf", &Y1) ;
			sscanf(opt7->answers[1], "%lf", &Y0) ;
			sscanf(opt7->answers[2], "%lf", &X0) ;
			sscanf(opt7->answers[3], "%lf", &X1) ;
			x0 = l+(int)((r-l)*X0/100.);
			x1 = l+(int)((r-l)*X1/100.);
			y0 = t+(int)((b-t)*(100.-Y0)/100.);	/* make lower left the origin */
			y1 = t+(int)((b-t)*(100.-Y1)/100.);
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

	if(x0 == x1)  x1++;	/* avoid 0 width boxes */
	if(y0 == y1)  y1++;

	if((x0 < l) || (x1 > r) || (y0 < t) || (y1 > b))	/* for mouse or at= 0- or 100+; needs to be after order check */
		fprintf(stderr, "Warning: legend box lies outside of frame. Text may not display properly.\n");

	horiz = (x1-x0 > y1-y0);
	if(horiz)
		fprintf(stderr, "Drawing horizontal legend as box width exceeds height.\n");
	
	if(!fp && horiz)	/* better than nothing */
		do_smooth = TRUE;
	

	MaxLabelLen = 0;	/* init variable */

	/* How many categories to show */
	if(!fp) {
		if (G_read_range(map_name, mapset, &range) == -1) {
			sprintf(buff,"Range information for [%s] not available (run r.support)", map_name);
			G_fatal_error(buff) ;
		}
		G_get_range_min_max (&range, &min_ind, &max_ind);

		G_get_color_range(&min_colr, &max_colr, &colors);

		if(UserRange) {
			if(min_ind < UserRangeMin)
				min_ind = (int)ceil(UserRangeMin);
			if(max_ind > UserRangeMax)
				max_ind = (int)floor(UserRangeMax);
			if(min_ind > UserRangeMin) {
				min_ind = UserRangeMin < min_colr ? min_colr : (int)ceil(UserRangeMin);
				G_warning("Color range exceeds lower limit of actual data");
			}
			if(max_ind < UserRangeMax) {
				max_ind = UserRangeMax > max_colr ? max_colr : (int)floor(UserRangeMax);
				G_warning("Color range exceeds upper limit of actual data");
			}
		}

		/*  cats_num is total number of categories in raster                  */
		/*  do_cats is  total number of categories to be displayed            */
		/*  k is number of cats to be displayed after skipping unlabeled cats */
		/*  lines is number of text lines/legend window                       */
 
		cats_num = max_ind - min_ind + 1 ;

		if (lines == 0) lines = cats_num ;

		do_cats = cats_num > lines ? lines : cats_num ;

		if(do_cats == cats_num)
			lines = (int)ceil((1.0*lines)/thin);
	
		if (!use_catlist) {
			catlist= (double *) G_calloc(lines+1, sizeof(double));
			catlistCount=lines;
		}
		/* see how many boxes there REALLY will be */
		maxCat = 0.0;
		for(i=min_ind, j=1, k=0; j<=do_cats && i<=max_ind; j++, i+=thin) {
		    if(!flip)
			cstr = G_get_cat(i, &cats);
		    else
			cstr = G_get_cat((max_ind - (i-min_ind)), &cats);

		    if (!use_catlist)
			catlist[j-1] = (double)i;

		    if(!cstr[0]) {	/* no cat label found, skip str output */
			if(hide_nodata)
			    continue;
		    }
		    else {		/* ie has a label */
			if( !hide_catstr && (MaxLabelLen < strlen(cstr)) )
			    MaxLabelLen=strlen(cstr);
		    }
		    
		    if(!hide_catnum)
			if(i > maxCat) maxCat = (double)i;
		    k++;    /* count of actual boxes drawn (hide_nodata option invaidates using j-1) */
		}
		lines = k;

		/* figure out how long the category + label will be */
		if(use_catlist) {
			MaxLabelLen=0;
			maxCat=0;	/* reset */
			for(i=0, k=0; i<catlistCount; i++) {
				if( (catlist[i] < min_ind) || (catlist[i] > max_ind) ) {
					sprintf(buff,"use=%s out of range [%d,%d]. (extend with range= ?)",
						opt8->answers[i], min_ind, max_ind);
					G_fatal_error(buff);
				}

				cstr = G_get_cat(catlist[i], &cats);
				if(!cstr[0]) {  /* no cat label found, skip str output */
					if(hide_nodata)
						continue;
				}
				else {		/* ie has a label */
					if( !hide_catstr && (MaxLabelLen < strlen(cstr)) )
						MaxLabelLen=strlen(cstr);
				}
				if(!hide_catnum)
					if(catlist[i] > maxCat) maxCat = catlist[i];
				k++;
			}
			if (0 == k)	/* nothing to draw */
				lines = 0;
		}

		if(MaxLabelLen > 0) {		/* ie we've picked up at least one label */
			MaxLabelLen++;			/* compensate for leading space */
			if(!hide_catnum)
				MaxLabelLen +=3;	/* compensate for "%2d) " */
		}
		else {
			if(!hide_catnum)
				MaxLabelLen=1;
		}

		/* compensate for categories >100 */
		if(!hide_catnum) {
		    if(maxCat > 99)
			MaxLabelLen += (int)(log10(maxCat));
		}

		/* following covers both the above if(do_cats == cats_num) and k++ loop */
		if(lines < 1) {
			lines = 1;	/* ward off the dpl floating point exception */
			G_fatal_error("Nothing to draw! (no categories with labels? out of range?)");
		}

		/* Figure number of lines, number of pixles per line and text size */
		dots_per_line = ((y1 - y0) / lines);

		/* switch to a smooth legend for CELL maps with too many cats */
		/*  an alternate solution is to set   dots_per_line=1         */
		if ((dots_per_line == 0) && (do_smooth == 0)) {
		    if(!use_catlist) {
			fprintf(stderr,
			    "Forcing a smooth legend: too many categories for current window height.\n");
			do_smooth = 1;
		    }
		}

		/* center really tiny legends */
		if( !use_mouse && opt7->answer == NULL)	{	/* if defualt scaling */
			if( !do_smooth && (dots_per_line < 4) )	/* if so small that there's no box */
				if( (b-(dots_per_line*lines))/(b*1.0) > 0.15)	/* if there's more than a 15% blank at the bottom */
					y0 = ((b-t) - (dots_per_line*lines))/2;
		}

	/*	R_text_size((int)(dots_per_line*4/5), (int)(dots_per_line*4/5)) ;    redundant */
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

		G_get_d_color_range(&min_dcolr, &max_dcolr, &colors);

		if(UserRange) {
			if(dmin < UserRangeMin)
				dmin = UserRangeMin;
			if(dmax > UserRangeMax)
				dmax = UserRangeMax;
			if(dmin > UserRangeMin) {
				dmin = UserRangeMin < min_dcolr ? min_dcolr : UserRangeMin;
				G_warning("Color range exceeds lower limit of actual data");
			}
			if(dmax < UserRangeMax) {
				dmax = UserRangeMax > max_dcolr ? max_dcolr : UserRangeMax;
				G_warning("Color range exceeds upper limit of actual data");
			}
		}

		if(use_catlist) {
			for(i=0; i<catlistCount; i++) {
				if( (catlist[i] < dmin) || (catlist[i] > dmax) ) {
					sprintf(buff,"use=%s out of range [%.3f, %.3f]. (extend with range= ?)",
						opt8->answers[i], dmin, dmax);
					G_fatal_error(buff);
				}
				if( strlen(opt8->answers[i]) > MaxLabelLen )
				    MaxLabelLen=strlen(opt8->answers[i]);
			}
		}
		do_cats = 0;	/* if only to get rid of the compiler warning  */
		cats_num = 0;	/* if only to get rid of the compiler warning  */
	}

	if(use_catlist) {
		cats_num = catlistCount;
		do_cats = catlistCount;
		lines = catlistCount;
		do_smooth = 0;
	}

	if(do_smooth){
	    int wleg, lleg, dx, dy;
	    int txsiz;
	    int ppl;
	    int tcell;
	    float ScaleFactor = 1.0;

	    if(horiz) {
		lleg = x1-x0;
		dx = 0;
		dy = y1-y0;
		if(fp)
		    flip = !flip;	/* horiz floats look better not flipped by default */
	    }
	    else {
		lleg = y1-y0;
		dy = 0;
		dx = x1-x0;
	    }

	    /* Draw colors */
	    R_move_abs(x0, y0);
	    for (k = 0; k < lleg; k++) {
		if (!fp) {
		    if(!flip)
			tcell = min_ind + k * (double)(1 + max_ind - min_ind)/lleg;
		    else
			tcell = (max_ind+1) - k * (double)(1 + max_ind - min_ind)/lleg;
		    D_color((CELL)tcell,&colors);
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

	    /* Format text */
	    if(!fp) {				/* cut down labelnum so they don't repeat */
		if(do_cats < steps)
		    steps = do_cats;
		if(1 == steps) steps = 2;	/* ward off the ppl floating point exception */ 
	    }

	    for(k = 0; k< steps; k++) {
		if(!fp) {
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
		    else {
			if(! hide_catnum && hide_catstr) /* num only */
			    sprintf(buff, "%2d", tcell);
			else {
			    if(hide_catnum && hide_catstr) /* nothing, box only */
			       buff[0] = 0;
			    else
			       sprintf(buff, "%2d) %s", tcell, cstr); /* both */
			}
		    }
		}
		else {   /* ie FP map */
		    if(!flip)
			val = dmin + k * (dmax - dmin)/(steps-1);
		    else
			val = dmax - k * (dmax - dmin)/(steps-1); 

		    if( 0 == (dmax - dmin) )		/* trap divide by 0 for single value rasters */
			sprintf(buff, "%f", val);
		    else {
			/* determine how many significant digits to display based on range */
			SigDigits = (int)ceil(log10(fabs(25/(dmax - dmin))));
			if(SigDigits < 0)
			    SigDigits = 0;
			if(SigDigits < 7)
			    sprintf(DispFormat, "%%.%df", SigDigits);
			else
			    sprintf(DispFormat, "%%.2g");	/* eg 4.2e-9  */
			sprintf(buff, DispFormat, val);
		    }
		}

		/* this probably shouldn't happen mid-loop as text sizes 
			might not end up being uniform, but it's a start */
		if( strlen(buff) > MaxLabelLen )
		    MaxLabelLen=strlen(buff);

		/* Draw text */
		if(!horiz)
		    txsiz = (int)((y1-y0)/20);
		else
		    txsiz = (int)((x1-x0)/20);

		/* scale text to fit in window if position not manually set */
		/* usually not needed, except when frame is really narrow   */
		if( !use_mouse && opt7->answer == NULL)	{	/* ie defualt scaling */
		    ScaleFactor = ((r-x1)/((MaxLabelLen+1)*txsiz*0.81)); /* ?? txsiz*.81=actual text width. */
		    if( ScaleFactor < 1.0) {
			txsiz = (int)(txsiz*ScaleFactor);
		    }
		}

		if(txsiz < 0) txsiz = 0;  /* keep it sane */

		R_text_size(txsiz, txsiz);
		R_standard_color(color);

		ppl = (lleg)/(steps-1);

		if(!horiz) {
		    if(!k) /* first  */
			R_move_abs(x1+4, y0+txsiz);
		    else if(k == steps-1) /* last */
			R_move_abs(x1+4, y1);
		    else
			R_move_abs(x1+4, y0+ppl*k + txsiz/2);
		}
		else {
		/* text width is 0.81 of text height? so even though we set width 
			to txsiz with R_text_size(), we still have to reduce.. hmmm */
		    if(!k) /* first  */
			R_move_abs(x0-(strlen(buff)*txsiz*.81/2), y1+4+txsiz);			
		    else if(k == steps-1) /* last */
			R_move_abs(x1-(strlen(buff)*txsiz*.81/2), y1+4+txsiz);
		    else
			R_move_abs(x0 +ppl*k -(strlen(buff)*txsiz*.81/2), y1+4+txsiz);
		}

		if(! hide_catnum)
		    R_text(buff);

	    } /*for*/

	    lleg = y1-y0;
	    wleg = x1-x0;

	    /* Black box */
	    R_standard_color(black);
	    R_move_abs(x0+1, y0+1);
	    R_cont_rel(0,lleg-2);
	    R_cont_rel(wleg-2, 0);
	    R_cont_rel(0, 2-lleg);
	    R_cont_rel(2-wleg, 0);

	    /* White box */
	    R_standard_color(white);
	    R_move_abs(x0, y0);
	    R_cont_rel(0,lleg);
	    R_cont_rel(wleg, 0);
	    R_cont_rel(0, -lleg);
	    R_cont_rel(-wleg, 0);

	}
	else{   /* non FP, no smoothing */

		int txsiz, true_l, true_r;
		float ScaleFactor = 1.0;
		
		/* set legend box bounds */
		true_l = l;
		true_r = r;	/* preserve window width */
		l=x0;
		t=y0;
		r=x1;
		b=y1;

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
			ScaleFactor = ((true_r-true_l)/((MaxLabelLen+3)*txsiz*0.81));	/* ?? txsiz*.81=actual text width. */
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
		y_box[1] = (5-dots_per_line) ;
		x_box[2] = (dots_per_line-5) ;
		y_box[2] = 0                 ;
		x_box[3] = 0                 ;
		y_box[3] = (dots_per_line-5) ;
		x_box[4] = (5-dots_per_line) ;
		y_box[4] = 0                 ;


		/* Draw away */

/*		if(ScaleFactor < 1.0)   */
/*		    cur_dot_row = ((b-t) - (dots_per_line*lines))/2; */ /* this will center the legend */
/*		else	*/
		 cur_dot_row = t + dots_per_line/2;

		/*  j = (do_cats == cats_num ? 1 : 2 ); */

		for(i=0, k=0; i<catlistCount; i++)
/*		for(i=min_ind, j=1, k=0; j<=do_cats && i<=max_ind; j++, i+=thin)	*/
		{
		    if(!flip)
			cstr = G_get_cat(catlist[i], &cats);
	            else
			cstr = G_get_cat(catlist[catlistCount-i-1], &cats);
		    

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
		    if(!fp) {
			if(!flip)
				D_color((CELL)(int)catlist[i],&colors) ;
			else
				D_color((CELL)(int)catlist[catlistCount-i-1],&colors) ; 
		    }
		    else {
			if(!flip)
				D_d_color(catlist[i],&colors);
			else
				D_d_color(catlist[catlistCount-i-1],&colors);
		    }
		    
		    R_move_abs(l+4, (cur_dot_row-2)) ;
		    R_polygon_rel(x_box, y_box, 5) ;

		    /* Draw text */
		    R_standard_color(color) ;

		    if(!fp) {
			if(hide_catnum && ! hide_catstr) /* str only */
				sprintf(buff, " %s", cstr);
			else {
				if(! hide_catnum && hide_catstr) { /* num only */
				    if(!flip)			
					sprintf(buff, "%2d", (int)catlist[i]);
				    else
					sprintf(buff, "%2d", (int)catlist[catlistCount-i-1]);
				}
				else{
					if(hide_catnum && hide_catstr) /* nothing, box only */
						buff[0] = 0;
					else {
						if(!flip)
						    sprintf(buff, "%2d) %s", (int)catlist[i], cstr); /* both */
						else
						    sprintf(buff, "%2d) %s", (int)catlist[catlistCount-i-1], cstr);
					}
				}
			}
		    }
		    else {	/* is fp */
			if(!flip)
				sprintf(buff, "%s", opt8->answers[i]);
			else
				sprintf(buff, "%s", opt8->answers[catlistCount-i-1]);
		    }

		    R_move_abs((l+3+dots_per_line), (cur_dot_row)-3);
		    R_text(buff);
		}

		if(0 == k) {
			sprintf(buff,"Nothing to draw! (no categories with labels?)");	/* "(..., out of range?)" */
			G_fatal_error(buff) ;
		}

		if (do_cats != cats_num)
		{
		    cur_dot_row += dots_per_line;
	/*	    sprintf(buff, "%d of %d categories\n", (j-1), cats_num) ;	*/

		    sprintf(buff, "%d of %d categories\n", k, cats_num) ;
		
		    /* shrink text if it will run off the screen */
		    MaxLabelLen=strlen(buff)+4;
		    ScaleFactor = ((true_r-true_l)/(MaxLabelLen*txsiz*0.81));	/* ?? txsiz*.81=actual text width. */
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
