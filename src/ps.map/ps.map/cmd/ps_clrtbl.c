/* Function: ps_colortable
**
** Author: Paul W. Carlson	April 1992
*/

#include "ps_info.h"
#include "colortable.h"

#define NSTEPS 5 /* number of steps to divide color box when showing color for
		    category data range */
extern int verbose;

int ps_colortable (void)
{
    char buf[512], *label;
    int num_cats;
    int i, j, k, jj;
    int R, G, B;
    int center_cols;
    DCELL dmin, dmax, val;
    double t, l, r;
    double x1, x2, y, dy, fontsize, tl;
    double col_width;

    /* let user know what's happenning */
    if (verbose > 1)
    {
        fprintf (stdout,"PS-PAINT: creating color table for <%s in %s> ...",
	    PS.cell_name, PS.cell_mapset);
        fflush(stdout);
    }

    if (G_read_cats(PS.cell_name, PS.cell_mapset, &PS.cats) == -1)
    {
        sprintf(buf, "Category file for [%s] not available", PS.cell_name);
        G_warning(buf);
        return 1;
    }

    /* set font */
    fontsize = (double)ct.fontsize;
    fprintf(PS.fp, "(%s) FN %.1f SF\n", ct.font, fontsize);

    /* set colortable location */
    dy = 1.5 * fontsize;
    if (ct.y <= 0.0) t = PS.min_y;
    else t = 72.0 * ( PS.page_height - ct.y);
    if (ct.x <= 0.0) ct.x = PS.left_marg;
    l = 72.0 * ct.x + 0.5;
    if (ct.width <= 0.0 || ct.width > PS.page_width  - PS.right_marg - ct.x)
        ct.width = PS.page_width  - PS.right_marg - ct.x;
    r  = l + 72.0 * ct.width;
    col_width = ct.width / (double)ct.cols;

    /* How many categories to show */
    num_cats = G_number_of_raster_cats(&PS.cats);

    /* read cats into PostScript array "a" */
    fprintf(PS.fp, "/a [\n");
    for(i = 0; i <= num_cats; i++)
    {
	if ( !i && !ct.nodata )  i++; /* step over 'no data' */
	if(!i)
	    fprintf(PS.fp, "(%s)\n", "no data");
        else
            fprintf(PS.fp, "(%s)\n", 
		  G_get_ith_d_raster_cat (&PS.cats, i-1, &dmin, &dmax));
    }		  
    fprintf(PS.fp, "] def\n");

    /* get width of widest string in PostScript variable "mw" */
    fprintf(PS.fp, "/mw 0 def 0 1 a length 1 sub { /i XD\n");
    fprintf(PS.fp, "a i get SW pop /t XD t mw gt {/mw t def} if } for\n");

    /* shrink font size to fit in width */
    if(ct.cols == 1) tl = 72.0 * col_width - 2.0 * fontsize; 
    else tl = 72.0 * col_width - 4.0 * fontsize; 
    fprintf(PS.fp, "/s %.1f def\n", fontsize);
    fprintf(PS.fp, "mw %.1f gt {/s s %.1f mul mw div def } if\n", tl, tl);
    fprintf(PS.fp, "(%s) FN s SF\n", ct.font);

    /* make proc to center multiple columns */
    center_cols = (ct.cols > 1);
    if (center_cols)
    {
    	fprintf(PS.fp, "/k %d def\n", ct.cols - 1);
    	fprintf(PS.fp, "/mlw 0 def 0 k a length 1 sub { /i XD\n");
    	fprintf(PS.fp, "a i get SW pop /t XD t mlw gt {/mlw t def} if } for\n");
	fprintf(PS.fp, "/xo mw mlw sub D2 s mul %1.0f div %1.0f add def\n", 
		fontsize, fontsize);
	fprintf(PS.fp, "/mvx {xo add} BD\n");
    }

    y = t - fontsize;
    k = 0;
    for(i = 0; i <= num_cats;)
    {
	if ( !i && !ct.nodata ) i++; /* step over 'no data' */

	/* test for bottom of page */
	y -= dy;
	if (y < 72.0 * PS.bot_marg) 
	{
	    y = 72.0 * (PS.page_height - PS.top_marg) - 0.5 * fontsize;
	    fprintf(PS.fp, "showpage\n");
	}

	for (j = 0; j < ct.cols; j++)
	{
	    /* get the data range */

	    /* fill box and outline in black */
	    if(i)
	       label = G_get_ith_d_raster_cat (&PS.cats, i-1, &dmin, &dmax);

	    x1 = l + (double)j * 72.0 * col_width;
	    x2 = x1 + fontsize;

	    if(!i || dmax==dmin)
	    /* draw a 1-color rectangle */
	    {
	       /* set box fill color */
	       if(!i)
		  G_get_null_value_color(&R, &G, &B, &PS.colors);
               else
	          G_get_d_raster_color(&dmin, &R, &G, &B, &PS.colors);
	       fprintf(PS.fp, "%.2f %.2f %.2f C\n", 
	   	  (double)R/255., (double)G/255., (double)B/255.);
               fprintf(PS.fp, "%.1f ", x1);
	       if (center_cols) fprintf(PS.fp, "mvx ");
	       fprintf(PS.fp, "%.1f ", y);
	       fprintf(PS.fp, "%.1f ", x2);
	       if (center_cols) fprintf(PS.fp, "mvx ");
	       fprintf(PS.fp, "%.1f ", y + fontsize);
	       fprintf(PS.fp, "B F BW stroke\n");
            }
	    else
	    /* split the rectangle into NSTEPS horisontal strips and
	       draw each with the corresponding value's color */
	    {
	       for(jj=0; jj<NSTEPS; jj++)
	       {
   	          /* set box fill color */
	          val = dmin + (double) jj * (dmax-dmin)/NSTEPS;
	          G_get_d_raster_color(&val, &R, &G, &B, &PS.colors);
		  fprintf(PS.fp, "%.2f %.2f %.2f C\n",
			  (double)R/255., (double)G/255., (double)B/255.);
    	          fprintf(PS.fp, "%.1f ", x1);
    	          if (center_cols) fprintf(PS.fp, "mvx ");
    	          fprintf(PS.fp, "%.1f ", 
			 y + (fontsize * (double) jj) / NSTEPS);
    	          fprintf(PS.fp, "%.1f ", x2);
    	          if (center_cols) fprintf(PS.fp, "mvx ");
    	          fprintf(PS.fp, "%.1f ", 
			 y + (fontsize * (double) (jj+1)) / NSTEPS);
    	          fprintf(PS.fp, "B CF stroke\n");
	       } /* done filling the box */

	    /* outline the box in black */
               fprintf(PS.fp, "%.1f ", x1);
	       if (center_cols) fprintf(PS.fp, "mvx ");
	       fprintf(PS.fp, "%.1f ", y);
	       fprintf(PS.fp, "%.1f ", x2);
	       if (center_cols) fprintf(PS.fp, "mvx ");
	       fprintf(PS.fp, "%.1f ", y + fontsize);
	       fprintf(PS.fp, "B BW stroke\n");
	    } /* done drawing the box */

	    /* do the text */
            fprintf(PS.fp, "a %d get %.1f ", k++, x1 + 2.0 * fontsize);
    	    if (center_cols) fprintf(PS.fp, "mvx ");
            fprintf(PS.fp, "%.1f MS\n", y);
            i++;
	    if (i >= num_cats) j = ct.cols + 1;
	}
    }
    y -= dy;
    if (PS.min_y > y) PS.min_y = y;

    if (verbose > 1) fprintf (stdout,"\n");

    return 0;
}
