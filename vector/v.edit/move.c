/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa
 *
 * PURPOSE:    This module edits vector maps. 
 *             Deletes selected features.
 *
 * COPYRIGHT:  (C) 2002-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include "global.h"

/* 
 * move selected features
 * return number of selected features
 * return -1
 */
int do_move(struct Map_info *Map, struct ilist *List, int print,
	    double move_x, double move_y)
{
    struct line_pnts *Points;
    struct line_cats *Cats;
    int i, j;
    int type, newline, line;
    int nlines_moved;

    nlines_moved = 0;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    for (i = 0; i < List->n_values; i++) {
	line = List -> value[i];

	if (!Vect_line_alive (Map, line))
	    continue;

        type = Vect_read_line(Map, Points, Cats, line);

        G_debug(3, "Moving type %d number %d", type, line);

        /* move */
        for (j = 0; j < Points -> n_points; j++) {

            Points->x[j] += move_x;
            Points->y[j] += move_y;

        } /* for each point at line */

	newline = Vect_rewrite_line (Map, line, type, Points, Cats);

        if (newline < 0)  {
	  G_warning(_("Cannot rewrite line %d"),
		    line);
            return -1;
        }

        nlines_moved++;
	
        if (print) {
	    fprintf(stdout, "%d%s",
		    line,
		    i < List->n_values -1 ? "," : "");
	    fflush (stdout);
	}
    }
        
    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);

    G_message(_("%d features moved"), nlines_moved);

    return nlines_moved;
}
