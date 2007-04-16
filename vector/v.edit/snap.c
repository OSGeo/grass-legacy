/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Martin Landa
 *
 * PURPOSE:    This module edits vector maps. 
 *             Snap *two* selected lines
 *
 * COPYRIGHT:  (C) 2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO: Snap more lines at the time
 *
 ****************************************************************/

#include "global.h"

/* 
 * snap *two* selected lines
 * return 1 on successs
 * return -1 on error
 */
int do_snap(struct Map_info *Map, struct ilist *List, int print)
{
    struct line_pnts *Points1, *Points2;
    struct line_cats *Cats1, *Cats2;
    int line1, line2, type1, type2;
    double mindist;
    int mindistidx;
    
    Points1 = Vect_new_line_struct();
    Cats1 = Vect_new_cats_struct();
    Points2 = Vect_new_line_struct();
    Cats2 = Vect_new_cats_struct();

    if (List->n_values != 2) {
        G_message(_("Cannot snap selected lines. Only 2 lines can be snapped at the time,"
		    "but [%d] lines selected"), List->n_values);
        return -1;
    }

    line1 = List -> value[0];
    line2 = List -> value[1];
    type1 = Vect_read_line(Map, Points1, Cats1, line1);
    type2 = Vect_read_line(Map, Points2, Cats2, line2);

    /* find mininal distance and its index */
    mindist = min_distance_line (Points1, Points2,
				 &mindistidx);

    /* set indexes and other things */
    switch(mindistidx) {
        /* for each mindistidx create new line */
        case 0: 
                Points2->x[0] = Points1->x[0];
                Points2->y[0] = Points1->y[0];
                Points2->z[0] = Points1->z[0];
                break;
        case 1: 
                Points2->x[Points2->n_points-1] = Points1->x[0];
                Points2->y[Points2->n_points-1] = Points1->y[0];
                Points2->z[Points2->n_points-1] = Points1->z[0];
                break;
        case 2: 
                Points2->x[0] = Points1->x[Points1->n_points-1];
                Points2->y[0] = Points1->y[Points1->n_points-1];
                Points2->z[0] = Points1->z[Points1->n_points-1];
                break;
        case 3: 
                Points2->x[Points2->n_points-1] = Points1->x[Points1->n_points-1];
                Points2->y[Points2->n_points-1] = Points1->y[Points1->n_points-1];
                Points2->z[Points2->n_points-1] = Points1->z[Points1->n_points-1];
                break;
    }

    if ( Vect_rewrite_line (Map, line2, type2, Points2, Cats2) < 0)  {
	G_warning(_("Cannot snap lines [%d,%d]"), line1, line2);
        return -1;
    }

    if (print) 
        fprintf(stdout,"%d,%d\n", line1, line2);

    G_message(_("Line [%d] snaped to line [%d]"), line2, line1);

    return 1;
}
