/* Function: ps_colortable
**
** Author: Paul W. Carlson	April 1992
*/

#include "ps_info.h"
#include "colortable.h"

extern int verbose;

ps_colortable()
{
    char buf[512];
    int num_cats;
    int color ;
    int i, j, k;
    int R, G, B;
    int center_cols;
    double t, l, r;
    double x1, x2, y, dy, fontsize, tl;
    double col_width;
    struct Range range;

    /* let user know what's happenning */
    if (verbose > 1)
    {
        printf("PS-PAINT: creating color table for <%s in %s> ...",
	    PS.cell_name, PS.cell_mapset);
        fflush(stdout);
    }

    if (G_read_cats(PS.cell_name, PS.cell_mapset, &PS.cats) == -1)
    {
        sprintf(buf, "Category file for [%s] not available", PS.cell_name);
        G_warning(buf);
        return 1;
    }

    if (G_read_range(PS.cell_name, PS.cell_mapset, &range) == -1)
    {
        sprintf(buf, "Range info for [%s] not available", PS.cell_name);
        G_warning(buf);
        return 1;
    }

    /* set font */
    fontsize = (double)ct.fontsize;
    fprintf(PS.fp, "(%s) FN %.1lf SF\n", ct.font, fontsize);

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
    num_cats = range.pmax - range.nmin + 1 ;

    /* read cats into PostScript array "a" */
    fprintf(PS.fp, "/a [\n");
    for(i = range.nmax; i <= range.pmax; i++)
    {
        if (!i) fprintf(PS.fp, "(no data)\n");
        else fprintf(PS.fp, "(%s)\n", G_get_cat(i, &PS.cats));
    }
    fprintf(PS.fp, "] def\n");

    /* get width of widest string in PostScript variable "mw" */
    fprintf(PS.fp, "/mw 0 def 0 1 a length 1 sub { /i XD\n");
    fprintf(PS.fp, "a i get SW pop /t XD t mw gt {/mw t def} if } for\n");

    /* shrink font size to fit in width */
    if(ct.cols == 1) tl = 72.0 * col_width - 2.0 * fontsize; 
    else tl = 72.0 * col_width - 4.0 * fontsize; 
    fprintf(PS.fp, "/s %1.lf def\n", fontsize);
    fprintf(PS.fp, "mw %.1lf gt {/s s %.1lf mul mw div def } if\n", tl, tl);
    fprintf(PS.fp, "(%s) FN s SF\n", ct.font);

    /* make proc to center multiple columns */
    center_cols = (ct.cols > 1);
    if (center_cols)
    {
    	fprintf(PS.fp, "/k %d def\n", ct.cols - 1);
    	fprintf(PS.fp, "/mlw 0 def 0 k a length 1 sub { /i XD\n");
    	fprintf(PS.fp, "a i get SW pop /t XD t mlw gt {/mlw t def} if } for\n");
	fprintf(PS.fp, "/xo mw mlw sub D2 s mul %1.lf div %1.lf add def\n", 
		fontsize, fontsize);
	fprintf(PS.fp, "/mvx {xo add} BD\n");
    }

    y = t - fontsize;
    k = 0;
    for(i = range.nmax; i <= range.pmax; )
    {
	/* test for bottom of page */
	y -= dy;
	if (y < 72.0 * PS.bot_marg) 
	{
	    y = 72.0 * (PS.page_height - PS.top_marg) - 0.5 * fontsize;
	    fprintf(PS.fp, "showpage\n");
	}

	for (j = 0; j < ct.cols; j++)
	{

   	    /* set box fill color */
	    G_get_color(i++, &R, &G, &B, &PS.colors);
	    fprintf(PS.fp, "%.2lf %.2lf %.2lf C\n", 
		(double)R/255., (double)G/255., (double)B/255.);

	    /* fill box and outline in black */
	    x1 = l + (double)j * 72.0 * col_width;
	    x2 = x1 + fontsize;
    	    fprintf(PS.fp, "%.1lf ", x1);
    	    if (center_cols) fprintf(PS.fp, "mvx ");
    	    fprintf(PS.fp, "%.1lf ", y);
    	    fprintf(PS.fp, "%.1lf ", x2);
    	    if (center_cols) fprintf(PS.fp, "mvx ");
    	    fprintf(PS.fp, "%.1lf ", y + fontsize);
    	    fprintf(PS.fp, "B F BW stroke\n");

	    /* do the text */
            fprintf(PS.fp, "a %d get %.1lf ", k++, x1 + 2.0 * fontsize);
    	    if (center_cols) fprintf(PS.fp, "mvx ");
            fprintf(PS.fp, "%.1lf MS\n", y);
	    if (i > range.pmax) j = ct.cols + 1;
	}
    }
    y -= dy;
    if (PS.min_y > y) PS.min_y = y;

    if (verbose > 1) printf("\n");
}
