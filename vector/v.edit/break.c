/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym.cepicky gmail.com>
 *             Martin Landa <martin.landa gmail.com>
 *
 * PURPOSE:    This module edits vector maps. 
 *             Break / connect the line
 *
 * COPYRIGHT:  (C) 2006-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include "global.h"

/*
 * breaks (split) selected vector line on position(s) given by coord
 *
 * return number of modified lines
 * return -1 on error
 */
int do_break (struct Map_info *Map, struct ilist *List, int print,
	      struct Option* coord, double thresh)
{
    int i, j, k, l;
    int type, line, seg;
    int nlines_modified;
    double east, north;
    double px, py, spdist, lpdist, dist;
    double *x, *y, *z;

    struct line_pnts *Points, *Points2;
    struct line_cats *Cats;
    struct ilist *List_in_box;
    BOUND_BOX bbox;

    nlines_modified = 0;

    Points = Vect_new_line_struct();
    Points2 = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    List_in_box = Vect_new_list();

    for (i = 0; i < List -> n_values; i++) {
	line = List -> value[i];
	
	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

	if (!(type & GV_LINES))
	    continue;

	x = Points -> x;
	y = Points -> y;
	z = Points -> z;

	for (j = 0; coord -> answers[j]; j += 2) {
	    east  = atof (coord -> answers[j]);
	    north = atof (coord -> answers[j+1]);
	    
	    seg = Vect_line_distance (Points, east, north, 0.0,
				      WITHOUT_Z,
				      &px, &py, NULL,
				      &dist, &spdist, &lpdist);
	    
	    G_debug (3, "do_break: line=%d, x=%f, y=%f, px=%f, py=%f, seg=%d, dist=%f, spdist=%f, lpdist=%f",
		     line, east, north, px, py, seg, dist, spdist, lpdist);

	    if (spdist <= 0.0 ||
		spdist >= Vect_points_distance (x[seg], y[seg], z[seg], px, py, 0.0, WITHOUT_Z))
		continue;
	    
	    G_debug (3, "do_break: line=%d broken", line);

	    /* copy first line part */
	    for (l = 0; l < seg; l++) {
		Vect_append_point(Points2,
				  x[l], y[l], z[l]);
	    }
	    
	    /* add last vertex */
	    Vect_append_point(Points2, px, py, 0.0);
	    
	    /* rewrite the line */
	    if (Vect_rewrite_line (Map, line, type, Points2, Cats) < 0)  {
		G_warning(_("Cannot rewrite line [%d]"), line);
		return -1;
	    }
	    
	    Vect_reset_line (Points2);
	    
	    /* add given vertex */
	    Vect_append_point(Points2, px, py, 0.0);
	    
	    /* copy second line part */
	    for (l = seg; l < Points->n_points; l++) {
		Vect_append_point(Points2, 
				  x[l], y[l], z[l]);
	    }
	    
	    /* rewrite the line */
	    if ( Vect_write_line (Map, type, Points2, Cats) < 0)  {
		G_warning(_("Cannot rewrite line [%d]"), line);
		return -1;
	    }
	    
	    if (print) {
		fprintf(stdout, "%d%s",
			line,
			i < List->n_values -1 ? "," : "");
		fflush (stdout);
	    }
	    nlines_modified++;
	} /* for each bounding box */
    } /* for each selected line */

    G_message(_("[%d] lines broken"), nlines_modified);

    return nlines_modified;
}
