/* Function: map_setup
**
** Author: Paul W. Carlson	3/92
*/

#include "ps_info.h"
#include "group.h"
#include "local_proto.h"

extern int verbose;

int map_setup (void)
{
    int row, col, cells_per_inch;
    double w, h;
    long num_cells, limit;

    /* set top of map */
    if (PS.set_y < PS.min_y) PS.min_y = PS.set_y;
    PS.map_y_orig = PS.min_y / 72.0;

    if (!PS.do_raster && !grp.do_group)
    {	
	/* if scale has been set... */
    	if (PS.scaletext[0]) 
    	{
	    /* if scaled map will fit in map limits... */
	    w = scale(PS.scaletext);
	    h = w * (PS.w.north - PS.w.south) / (PS.w.east  - PS.w.west);
	    if (w <= PS.map_width && h <= PS.map_height)
	    {
	    	PS.map_width  = w;
	    	PS.map_height = h;
        	PS.map_pix_wide = 72.0 * PS.map_width;
        	PS.map_pix_high = 72.0 * PS.map_height; 
	    }

	    /* else, kill the scale */
	    else PS.scaletext[0] = 0;
	}
	
	/* fit map to bounding box */
	fit_map_to_box();
    }

    else
    {
    	if (PS.scaletext[0]) 
    	{
	    /* if scaled map will fit in map limits... */
	    w = scale(PS.scaletext);
	    h = w * PS.w.ns_res * (double)PS.w.rows /
    		   (PS.w.ew_res * (double)PS.w.cols);
	    if (w <= PS.map_width && h <= PS.map_height)
	    {
	    	PS.map_width  = w;
	    	PS.map_height = h;
        	PS.map_pix_wide = 72.0 * PS.map_width;
        	PS.map_pix_high = 72.0 * PS.map_height; 
	    }

	    /* else, kill the scale */
	    else PS.scaletext[0] = 0;
	}
	
	/* fit map to bounding box */
	fit_map_to_box();

    	PS.cells_high = PS.w.rows;
    	PS.cells_wide = PS.w.cols;
    	PS.ew_res = PS.w.ew_res;
    	PS.ns_res = PS.w.ns_res;

    	/* adjust sampling if too big */
	cells_per_inch = (int)((double)PS.cells_wide / PS.map_width + 0.5);
    	num_cells = (long)PS.cells_high * (long)PS.cells_wide;
    	PS.row_delta = 1;
    	PS.col_delta = 1;
	if (PS.grey || PS.level == 1) limit = 300000000L;
	else limit = 100000000L;
    	while (num_cells > limit || cells_per_inch > PS.res)
    	{	
	    if (PS.cells_high >= PS.cells_wide)
            {   
	    	PS.cells_high /= 2;
	    	PS.ns_res *= 2.0;
	    	PS.row_delta++;
		cells_per_inch /= 2;
	    }
	    else
	    { 
	    	PS.cells_wide /= 2;
	    	PS.ew_res *= 2.0;
	    	PS.col_delta++;
		cells_per_inch /= 2;
	    }
    	    num_cells = (long)PS.cells_high * (long)PS.cells_wide;
    	}

    	/* adjust image rows and cols to match values in writing hex string */
    	for (PS.cells_high = row = 0; row < PS.w.rows; row++) 
	    if ((row % PS.row_delta) == 0) PS.cells_high++;
    	for (PS.cells_wide = col = 0; col < PS.w.cols; col += PS.col_delta) 
		PS.cells_wide++;

    	/* compute conversion factors */
    	PS.ew_to_x = PS.map_pix_wide  / (PS.w.east  - PS.w.west);
    	PS.ns_to_y = PS.map_pix_high  / (PS.w.north - PS.w.south);
    }

    /* set the scale */
    if (!PS.scaletext[0]) sprintf(PS.scaletext, "1 : %.0f",
	39.37 * 72.0 * (PS.w.east - PS.w.west) / PS.map_pix_wide);
    if (verbose > 1)
    {
        fprintf (stdout,"PS-PAINT: scale set to %s.\n", PS.scaletext);
        fflush(stdout);
    }

    /* compute map edges */
    PS.map_left  = 72.0 * PS.map_x_orig;
    PS.map_top   = 72.0 * PS.map_y_orig;
    PS.map_bot   = PS.map_top  - PS.map_pix_high;
    PS.map_right = PS.map_left + PS.map_pix_wide;
    PS.min_y = PS.map_bot;

    /* we want the size to be 10 times biger, because G_plot_where_xy()
       returns integer values (pixels) for x and y, and we want doubles
       until the first decimal point. Then in move() and cont() we will
       divide x and y by 10. to get double coordinates */
    G_setup_plot(PS.map_top * 10., PS.map_bot * 10., PS.map_left * 10., PS.map_right * 10., move_local, cont_local);

/* debug
    fprintf (stdout,"t %.1f b %.1f l %.1f r %.1f\n", PS.map_top, PS.map_bot, PS.map_left, PS.map_right);
    */

    /* save original graphics state */
    fprintf(PS.fp, "gsave ");

    /* compute conversion factor from meters to PostScript window coordinates */
    /*
    G_begin_distance_calculations();
    meters_to_PS = (PS.map_top - PS.map_bot) / G_distance(0., PS.w.south, 0., PS.w.north);
    */

    /* clip to edge of border */
    box_clip(PS.map_top  - 1.0, PS.map_bot   + 1.0, 
	     PS.map_left + 1.0, PS.map_right - 1.0);

    return 0;
}
