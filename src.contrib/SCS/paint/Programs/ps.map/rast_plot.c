/* Function: PS_raster_plot
**
** Author: Paul W. Carlson	3/92
*/

#include "ps_info.h"

extern int verbose;

PS_raster_plot()
{
    int n, r, g, b, row, col;
    CELL *cellbuf;

    if (!PS.do_raster) return;

    /* save graphics state */
    fprintf(PS.fp, "gsave\n");

    /* make variables for cells_wide and cells_high */
    fprintf(PS.fp, "/cw %d def /ch %d def\n", PS.cells_wide, PS.cells_high);

    /* define  strings to hold image RGB values */
    fprintf(PS.fp, "rgbstrings \n");

    /* set lower left corner of map */
    fprintf(PS.fp, "%.2lf %.2lf TR\n", PS.map_left, PS.map_bot);

    /* mapping of image to map_pix_wide x map_pix_high unit rectangle */
    fprintf(PS.fp, "%d %d scale\n", 
	(int)(PS.map_pix_wide + 0.5), (int)(PS.map_pix_high + 0.5));

    /* decide which PostScript level to use */
    fprintf(PS.fp, "level 1 eq {domap1} {domap2} ifelse\n");

    /* let user know what's happenning */
    if (verbose > 1)
    {
        printf("PS-PAINT: reading raster file <%s in %s> ...",
	    PS.cell_name, PS.cell_mapset);
        fflush(stdout);
    }

    /* build the image RGB string */
    cellbuf = G_allocate_cell_buf();
    n = 0;
    for (row = 0; row < PS.w.rows; row++)
    {
	G_get_map_row(PS.cell_fd, cellbuf, row);
	if ((row % PS.row_delta) == 0)
	{   for (col = 0; col < PS.w.cols; col += PS.col_delta) 
	    {   
	        G_get_color(cellbuf[col], &r, &g, &b, &PS.colors);
		fprintf(PS.fp, "%02X%02X%02X", r, g, b);
	        if (++n == 13)
	        {	
		    n = 0;
		    fprintf(PS.fp, "\n");
		}
	    }
	}
    }
    fprintf(PS.fp, "\n");

    /* we're done with the cell stuff */
    if (!PS.do_colortable) G_free_colors(&PS.colors);
    G_close_cell(PS.cell_fd);
    free(cellbuf);

    /* restore graphics state */
    fprintf(PS.fp, "grestore\n");

    if (verbose > 1) printf("\n");
}
