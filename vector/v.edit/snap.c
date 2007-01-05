/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *
 * PURPOSE:    This module edits vector maps. It is inteded to be mainly
 * 	       used by the the new v.digit GUI.
 *
 * COPYRIGHT:  (C) 2002-2006 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       
 ****************************************************************/

#include "global.h"

int do_snap(struct Map_info *Map)
{
    int type1, type2;
    struct line_pnts *Points1, *Points2, *NPoints;
    struct line_cats *Cats1, *Cats2;
    int line1,line2;
    double distances[4];
    int mindistidx;
    int i;
    struct ilist *List;

    Points1 = Vect_new_line_struct();
    Cats1 = Vect_new_cats_struct();
    Points2 = Vect_new_line_struct();
    Cats2 = Vect_new_cats_struct();

    /* select two lines defined by cats or coord or bbox */
    List = select_lines(Map);

    if (List->n_values <2) {
        G_message(_("Only [%d] lines found, two needed"),List->n_values);
        return 0;
    }

    line1 = List->value[0];
    line2 = List->value[1];
    type1 = Vect_read_line(Map, Points1, Cats1, line1);
    type2 = Vect_read_line(Map, Points2, Cats2, line2);

    /* array distances will hold distances between first and last point of both lines:
     * distances[0] = first-first
     * distances[1] = first-last
     * distances[2] = last-first
     * distances[3] = last-last
     */
    distances[0] = Vect_points_distance(Points1->x[0], Points1->y[0], Points1->z[0],
                              Points2->x[0], Points2->y[0], Points2->z[0],0);
    distances[1] = Vect_points_distance(Points1->x[0], Points1->y[0], Points1->z[0],
                              Points2->x[Points2->n_points-1], Points2->y[Points2->n_points-1], Points2->z[Points2->n_points-1],0);
    distances[2] = Vect_points_distance(Points1->x[Points1->n_points-1], Points1->y[Points1->n_points-1], Points1->z[Points1->n_points-1],
                              Points2->x[0], Points2->y[0], Points2->z[0],0);
    distances[3] = Vect_points_distance(Points1->x[Points1->n_points-1], Points1->y[Points1->n_points-1], Points1->z[Points1->n_points-1],
                              Points2->x[Points2->n_points-1], Points2->y[Points2->n_points-1], Points2->z[Points2->n_points-1],0);

    /* find the minimal distance between first or last point of both lines */
    mindistidx = 0;
    for (i = 0; i < 4; i++)
        if (distances[i] < distances[mindistidx])
            mindistidx = i;

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
        G_warning("Lines could not be snaped");
        return 0;
    }

    if (i_flg->answer) 
        fprintf(stdout,"%d\n", line2);

    G_message(_("Line [%d] snaped to line [%d]"), line2,line1);
    return 1;
}
