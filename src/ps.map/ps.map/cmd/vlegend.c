/* Function: vect_legend
**
** Author: Paul W. Carlson	April 1992
*/

#include "vector.h"
#include "ps_info.h"
#include "local_proto.h"

int vect_legend (void)
{
    int i;
    double x, y, fontsize, dy, margin;

    /* set font */
    fontsize = (double)vector.fontsize;
    fprintf(PS.fp, "(%s) FN %.1f SF\n", vector.font, fontsize);

    /* get text location */
    dy = 1.5 * fontsize;
    if (vector.x > 0.0) x = 72.0 * vector.x;
    else x = PS.map_left;
    if (vector.y > 0.0) y = 72.0 * (PS.page_height - vector.y);
    else if (vector.x <= 0.0) y = PS.min_y;
    else y = PS.map_bot;
    margin = 0.4 * fontsize;
    if (x < PS.map_left + margin) x = PS.map_left + margin;

    /* make PostScript array "a" of name-mapset strings */
    fprintf(PS.fp, "/a [\n");
    for (i = 0; i < vector.count; i++)
    {
        fprintf(PS.fp, "(  %s (%s))\n", vector.name[i], vector.mapset[i]);
    }
    fprintf(PS.fp, "] def\n");

    /* if vector legend is on map... */
    if (y > PS.map_bot && y <= PS.map_top && x < PS.map_right)
    {
	fprintf(PS.fp, "/mg %.1f def\n", margin);

        /* get width of widest string in PostScript variable "w" */
        fprintf(PS.fp, "/w 0 def 0 1 a length 1 sub { /i XD\n");
        fprintf(PS.fp, "a i get SW pop /t XD t w gt {/w t def} if } for\n");
    	fprintf(PS.fp, "/w w %.1f add mg add 72 add def\n", x);

    	/* make white background for text */
    	fprintf(PS.fp, "1 1 1 C ");
    	fprintf(PS.fp, "%.1f %.1f w %.1f B fill \n", 
		x - margin,  y - vector.count * dy - margin, y);
    }

    /* make the legend */
    for (i = 0; i < vector.count; i++)
    {
	y -= dy;

	/* make a grey box if needed */
	if ((vector.hwidth[i] > 0. && vector.hcolor[i] == WHITE) ||
	    (vector.hwidth[i] < 1. && vector.colors[i][0]  == WHITE))
	{
	    fprintf(PS.fp, "0.5 setgray ");
    	    fprintf(PS.fp, "%.1f %.1f %.1f %.1f B fill \n", 
		x, y, x + 72.0, y + fontsize);
	}

	/* do highlight, if any */
	if (vector.hwidth[i])
	{
	    set_rgb_color(vector.hcolor[i]);
	    fprintf(PS.fp, "%.8f W\n",  
		  vector.width[i] + 2 * vector.hwidth[i]);
	    fprintf(PS.fp, "[] 0 setdash\n");
	    fprintf(PS.fp, "%.1f %.1f %.1f %.1f L\n",
		x + 72.0, y + 0.5 * fontsize, x, y + 0.5 * fontsize);
	}

	/* plot the primary color line */
	set_rgb_color(vector.colors[i][0]);
	fprintf(PS.fp, "%.8f W\n", vector.width[i] );
	fprintf(PS.fp, "%s setdash\n", vector.setdash[i]);
	fprintf(PS.fp, "%.1f %.1f %.1f %.1f L\n",
	    x + 72.0, y + 0.5 * fontsize, x, y + 0.5 * fontsize);

	/* plot the text */
	set_rgb_color(BLACK);
	fprintf(PS.fp, "a %d get %.1f %.1f MS\n", i, x + 72.0, y);
    }
    fprintf(PS.fp, "[] 0 setdash\n");

    if (PS.min_y > y) PS.min_y = y;

    return 0;
}
