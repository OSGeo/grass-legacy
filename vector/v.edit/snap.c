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
 * alternative snapping function using library function
 * return 1
 */
int do_snap(struct Map_info *Map, struct ilist *List, double thresh, int layer,
	    int print, struct ilist *List_updated) {
    
    FILE * output;

    if (G_verbose() > G_verbose_min()) {
	G_important_message (SEP);
	output = stderr;
    }
    else
	output = NULL;

    Vect_snap_lines_list (Map, List, thresh, NULL, output);

    if (G_verbose() > G_verbose_min()) {
	G_important_message (SEP);
    }

    return 1;
}

/* 
 * snap *two* selected lines
 * return 1 on successs
 * return -1 on error
 */
int do_snap2(struct Map_info *Map, struct ilist *List, int layer,
	    int print, struct ilist *List_updated)
{
    struct line_pnts *Points1, *Points2;
    struct line_cats *Cats1, *Cats2;
    int line1, line2, type1, type2, cat1, cat2;
    int newline;
    double mindist;
    int mindistidx;
    
    Points1 = Vect_new_line_struct();
    Cats1 = Vect_new_cats_struct();
    Points2 = Vect_new_line_struct();
    Cats2 = Vect_new_cats_struct();

    if (List->n_values != 2) {
        G_message(_("Cannot snap selected lines. Only %d lines can be snapped at the time"), 2);
        return -1;
    }

    if (List_updated) {
	Vect_reset_list(List_updated);
    }

    line1 = List -> value[0];
    line2 = List -> value[1];
    type1 = Vect_read_line(Map, Points1, Cats1, line1);
    type2 = Vect_read_line(Map, Points2, Cats2, line2);

    /* find mininal distance and its indexes */
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

    newline = Vect_rewrite_line (Map, line2, type2, Points2, Cats2);
    if (newline < 0) {
	G_warning(_("Cannot snap lines %d,%d"), line1, line2);
        return -1;
    }
    
    if (List_updated) {
	Vect_list_append(List_updated, newline);
    }

    if (print) 
        fprintf(stdout,"%d,%d\n", line1, line2);

    Vect_cat_get (Cats1, layer, &cat1); /* if not found, cat1 is set to -1 */
    Vect_cat_get (Cats2, layer, &cat2);

    G_message(_("Line id/cat %d/%d snapped to line %d/%d"),
	      line2, cat2,
	      line1, cat1);

    return 1;
}
