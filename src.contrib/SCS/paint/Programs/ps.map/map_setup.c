/* Function: map_setup
**
** Author: Paul W. Carlson	3/92
*/

#include "ps_info.h"
extern int verbose;

map_setup()
{
    char buf[128];
    int row, col;
    double w, h, pix_per_cell;
    double scale();
    long num_cells;

    /* set top of map */
    if (PS.set_y < PS.min_y) PS.min_y = PS.set_y;
    PS.map_y_orig = PS.min_y / 72.0;

    if (!PS.do_raster)
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
	    }

	    /* else, kill the scale */
	    else PS.scaletext[0] = 0;
	}

	/* make map fit in bounding box */
    	PS.ew_to_x = PS.map_width  * 72.0 / (PS.w.east  - PS.w.west);
    	PS.ns_to_y = PS.map_height * 72.0 / (PS.w.north - PS.w.south);
	if (PS.ew_to_x < PS.ns_to_y) 
	{
	    PS.map_pix_wide = 72.0 * PS.map_width; 
	    PS.map_pix_high = 72.0 * PS.map_height * 
				PS.ew_to_x / PS.ns_to_y;
	    PS.ns_to_y = PS.ew_to_x;
	}
	else 
	{
	    PS.map_pix_wide = 72.0 * PS.map_width * PS.ns_to_y / PS.ew_to_x;
	    PS.map_pix_high = 72.0 * PS.map_height; 
	    PS.ew_to_x = PS.ns_to_y;
	}
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
	    }

	    /* else, kill the scale */
	    else PS.scaletext[0] = 0;
	}

    	PS.cells_high = PS.w.rows;
    	PS.cells_wide = PS.w.cols;
    	PS.ew_res = PS.w.ew_res;
    	PS.ns_res = PS.w.ns_res;

    	/* adjust sampling if too big */
    	num_cells = (long)PS.cells_high * (long)PS.cells_wide;
    	PS.row_delta = 1;
    	PS.col_delta = 1;
    	while (num_cells > 2000000L)
    	{	
	    if (PS.cells_high >= PS.cells_wide)
            {   
	    	PS.cells_high /= 2;
	    	PS.ew_res *= 2.0;
	    	PS.row_delta++;
	    }
	    else
	    { 
	    	PS.cells_wide /= 2;
	    	PS.ns_res *= 2.0;
	    	PS.col_delta++;
	    }
    	    num_cells = (long)PS.cells_high * (long)PS.cells_wide;
    	}

    	/* adjust image rows and cols to match values in writing hex string */
    	for (PS.cells_high = row = 0; row < PS.w.rows; row++) 
	    if ((row % PS.row_delta) == 0) PS.cells_high++;
    	for (PS.cells_wide = col = 0; col < PS.w.cols; col += PS.col_delta) 
		PS.cells_wide++;

	/* make map fit in bounding box */
    	if (PS.ew_res < PS.ns_res)
    	{
            w = PS.map_width * 72.0 / (double)PS.cells_wide;
            h = (PS.ns_res / PS.ew_res) * PS.map_height * 
		    72.0 / (double)PS.cells_high;
    	}
    	else
    	{
	    w = (PS.ew_res / PS.ns_res) * PS.map_width * 
		    72.0 /(double)PS.cells_wide;
            h = PS.map_height * 72.0 / (double)PS.cells_high;
    	}
    	pix_per_cell  = (w < h) ? w : h;
    	PS.map_pix_wide  = pix_per_cell * (double)PS.cells_wide;
    	PS.map_pix_high  = pix_per_cell * (double)PS.cells_high;

    	/* compute conversion factors */
    	PS.ew_to_x = PS.map_pix_wide  / (PS.w.east  - PS.w.west);
    	PS.ns_to_y = PS.map_pix_high  / (PS.w.north - PS.w.south);
    }

    /* set the scale */
    if (!PS.scaletext[0]) sprintf(PS.scaletext, "1 : %.0lf",
	39.37 * 72.0 * (PS.w.east - PS.w.west) / PS.map_pix_wide);
    if (verbose > 1)
    {
        printf("PS-PAINT: scale set to %s\n", PS.scaletext);
        fflush(stdout);
    }

    /* compute map edges */
    PS.map_left  = 72.0 * PS.map_x_orig;
    PS.map_top   = 72.0 * PS.map_y_orig;
    PS.map_bot   = PS.map_top  - PS.map_pix_high;
    PS.map_right = PS.map_left + PS.map_pix_wide;
    PS.min_y = PS.map_bot;

    /* save original graphics state */
    fprintf(PS.fp, "gsave ");

    /* clip to edge of border */
    box_clip(PS.map_top  - 1.0, PS.map_bot   + 1.0, 
	     PS.map_left + 1.0, PS.map_right - 1.0);
}
