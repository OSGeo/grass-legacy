/* Function: ps_map
**
** This function writes the PostScript output file.
**
** Author: Paul W. Carlson	March 1992
*/

#include <stdio.h>
#include <unistd.h>
#include "ps_info.h"
#include "vector.h"
#include "group.h"
#include "local_proto.h"

extern int verbose;
extern int do_mapinfo;
extern int do_vlegend;
extern int rotate_plot;
extern int ps_copies;

int ps_map (void)
{
    char *date;
    long current_offset;

    /* get date */
    date = G_date();

    /* write the PostScript header */
    write_PS_header();

    /* create the PostScript procs */
    make_procs();

    /* set number of copies */
    if (ps_copies > 1) fprintf(PS.fp, "/#copies %d def\n", ps_copies);

    /* rotate map? */
    if (rotate_plot)
    {
    	fprintf(PS.fp, "%.2f 0.0 TR\n", 72.0 * PS.page_height);
    	fprintf(PS.fp, "90 rotate\n");
    }

    /* do the map header */
    if (PS.do_header) do_map_header(date);

    /* size the map */
    map_setup();

    /* do the raster stuff, if any */
    if (PS.do_raster || grp.do_group) PS_raster_plot();

    /* do the outline, if requested */
    if (PS.do_outline) ps_outline();

    /* do the masked vector plots, if any */
    if (vector.count) do_vectors(0);

    /* do the masked ponts/lines, if any */
    do_plfile(0);

    /* do masking, if required */
    PS_make_mask();
    if (PS.mask_needed) 
        do_masking();

    /* do the grid, if any */
    do_grid();

    /* do the unmasked vector plots, if any */
    if (vector.count) do_vectors(1);

    /* do the sites, if any */
    do_sites();

    /* do the grid numbers, if any */
    if (PS.grid_numbers > 0) do_grid_numbers();

    /* do the labels from paint/labels, if any */
    do_labels(0);

    /* restore the unclipped graphics state */
    fprintf(PS.fp, "grestore ");

    /* do the unmasked points, lines and eps if any */
    do_plfile(1);

    /* do the labels specified in script file */
    do_labels(1);

    /* show the map info */
    if (do_mapinfo) map_info();

    /* show the vector legend */
    if (do_vlegend && vector.count) vect_legend();

    /* put border around map */
    fprintf(PS.fp, "BW\n");
    box_draw(PS.map_top  - 0.5, PS.map_bot   + 0.5, 
	     PS.map_left + 0.5, PS.map_right - 0.5);

    /* do the colortable, if requested */
    if (PS.do_colortable) ps_colortable();

    /* do comments, if any */
    if (PS.commentfile != NULL) do_comments();

    /* do any PostScript include files */
    if (PS.num_psfiles) do_psfiles();

    /* write the bounding box */
    current_offset = ftell(PS.fp);
    write_bounding_box();
    fseek(PS.fp, current_offset, SEEK_SET);

    fprintf(PS.fp, "showpage\n");
    fprintf(PS.fp, "%%%%Trailer\n");
    fprintf(PS.fp, "%%%%EOF\n");
    fclose(PS.fp);

    return 0;
}

