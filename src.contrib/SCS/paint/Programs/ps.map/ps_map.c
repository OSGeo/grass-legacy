
/* Function: ps_map
**
** This function writes the PostScript output file.
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"
#include "vector.h"

extern int verbose;

ps_map()
{
    if (verbose > 1)
    {
        printf("\n");
        fflush(stdout);
    }

    /* write the PostScript header */
    write_PS_header();

    /* create the PostScript procs */
    make_procs();

    /* do the map header */
    if (PS.do_header) do_map_header();

    /* size the map */
    map_setup();

    /* do the raster stuff, if any */
    if (PS.do_raster) PS_raster_plot();

    /* do the outline, if requested */
    if (PS.do_outline) ps_outline();

    /* do the vector plots, if any */
    if (vector.count) do_vectors();

    /* do the labels, if any */
    do_labels();

    /* restore the unclipped graphics state */
    fprintf(PS.fp, "grestore ");

    /* show the map info */
    map_info();

    /* show the vector legend */
    if (vector.count) vect_legend();

    /* put border around map */
    fprintf(PS.fp, "BW\n");
    box_draw(PS.map_top  - 0.5, PS.map_bot   + 0.5, 
	     PS.map_left + 0.5, PS.map_right - 0.5);

    /* do the colortable, if requested */
    if (PS.do_colortable) ps_colortable();

    fprintf(PS.fp, "showpage\n");
    fprintf(PS.fp, "%%%%Trailer\n");
    fprintf(PS.fp, "%%%%EOF\n");
    fclose(PS.fp);
}

