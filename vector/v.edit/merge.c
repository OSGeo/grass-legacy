/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Original author Jachym Cepicky <jachym  les-ejk cz>
 *             Updated by Martin Landa <landa.martin@gmail.com> (2007/03)
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

/*
 * merge two given lines a,b
 * a : Points1/Cats1
 * b : Points2/Cats2
 * merged line : Points/Cats
 *
 * return 1 on success
 * return 0 on error
*/
static int merge_lines (struct line_pnts *Points1, struct line_cats *Cats1, 
			struct line_pnts *Points2, struct line_cats *Cats2,
			int thresh, struct line_pnts **Points);

/*
 * merge lines in vector map
 *
 * return number of merged lines
 * return -1 on error
 */
int do_merge(struct Map_info *Map)
{
    struct ilist *List, *List_in_box;

    struct line_pnts *Points1, *Points2, *Points;
    struct line_cats *Cats1, *Cats2;
    
    int type1, type2;
    int line, line1, line2;
    int i, nlines_merged;

    double thresh;
    
    Points1 = Vect_new_line_struct();
    Cats1   = Vect_new_cats_struct();
    Points2 = Vect_new_line_struct();
    Cats2   = Vect_new_cats_struct();
    Points = Vect_new_line_struct();

    List_in_box = Vect_new_list();

    nlines_merged = 0;
    thresh = atof (maxdist_opt -> answer);

    /* select lines */
    List = select_lines(Map);
    
    if (List->n_values < 2) {
	G_message(_("Only [%d] lines found, at least two needed"), List->n_values);
        return 0;
    }
    
    /* snap lines 
       if (thresh > 0)
       Vect_snap_lines (Map, GV_LINES, thresh, NULL, NULL);
    */

    /* merge lines */
    for (line1 = 0; line1 < List -> n_values; line1++) {
	G_percent (line1, List -> n_values, 2);
	
	if (!Vect_line_alive (Map, line1))
	    continue;
	
	type1 = Vect_read_line (Map, Points1, Cats1, line1);
	
	if (!(type1 & GV_LINES))
	    continue;
	
	Vect_reset_line (Points);

	for (i = 0; i < Points1 -> n_points; i += Points1 -> n_points - 1) {
	    Vect_reset_list (List_in_box);
	    
	    /* define searching region */
	    Vect_reset_line (Points2);
	    Vect_append_point (Points2, Points1 -> x[i] - thresh , Points1 -> y[i] + thresh, Points1 -> z[i]);
	    Vect_append_point (Points2, Points1 -> x[i] + thresh , Points1 -> y[i] + thresh, Points1 -> z[i]);
	    Vect_append_point (Points2, Points1 -> x[i] + thresh , Points1 -> y[i] - thresh, Points1 -> z[i]);
	    Vect_append_point (Points2, Points1 -> x[i] - thresh , Points1 -> y[i] - thresh, Points1 -> z[i]);

	    /* 
	     * merge lines only if two lines found in the region
	     * i.e. the current line and an adjacent line
	     */
	    if (2 == Vect_select_lines_by_polygon (Map, Points2, 0, NULL, GV_LINES, List_in_box)) {
		if (List_in_box -> value [0] == line1)
		    line2 = List_in_box -> value [1];
		else
		    line2 = List_in_box -> value [0];
		
		if (!Vect_val_in_list (List, line2))
		    continue;

		type2 = Vect_read_line (Map, Points2, Cats2, line2);

		G_debug (3, "merge lines: %d, %d", line1, line2);
		
		merge_lines (Points1, Cats1,
			     Points2, Cats2,
			     thresh,  &Points);

		if (Points -> n_points > 0) {
		    if (i_flg->answer)
			fprintf(stdout,"%d%s", List->value[line1], line1 < List->n_values-1 ? "," : "\n");

		    Vect_delete_line(Map, line2);
		    nlines_merged++;
		}
	    }
	}	      
	
	if (Points -> n_points > 0) {
	    line = Vect_rewrite_line (Map, line1, type1, Points, Cats1);
	    if (line < 0)
		G_fatal_error (_("Lines could not be merged"));

	    /* update number of lines */
	    Vect_list_append (List, line);
	}
    } /* line1 */

    /* destroy stuctures */
    Vect_destroy_line_struct(Points1);
    Vect_destroy_line_struct(Points2);
    Vect_destroy_line_struct(Points);

    Vect_destroy_cats_struct(Cats1);
    Vect_destroy_cats_struct(Cats2);

    Vect_destroy_list(List);

    return nlines_merged;
}

static int merge_lines (struct line_pnts *Points1, struct line_cats *Cats1, 
			struct line_pnts *Points2, struct line_cats *Cats2,
			int thresh, struct line_pnts **Points)
{
    struct line_pnts *ps = *Points;
    struct line_cats *cs = Cats1;

    int i, mindistidx;
    double distances[4];

    /* array distances will hold distances between first and last point of both lines:
     * distances[0] = first-first
     * distances[1] = first-last
     * distances[2] = last-first
     * distances[3] = last-last
     */
    distances[0] = Vect_points_distance(Points1->x[0], Points1->y[0], Points1->z[0],
					Points2->x[0], Points2->y[0], Points2->z[0], 0);
    
    distances[1] = Vect_points_distance(Points1->x[0], Points1->y[0], Points1->z[0],
					Points2->x[Points2->n_points-1], Points2->y[Points2->n_points-1], Points2->z[Points2->n_points-1], 0);
    
    distances[2] = Vect_points_distance(Points1->x[Points1->n_points-1], Points1->y[Points1->n_points-1], Points1->z[Points1->n_points-1],
					Points2->x[0], Points2->y[0], Points2->z[0], 0);
    
    distances[3] = Vect_points_distance(Points1->x[Points1->n_points-1], Points1->y[Points1->n_points-1], Points1->z[Points1->n_points-1],
					Points2->x[Points2->n_points-1], Points2->y[Points2->n_points-1], Points2->z[Points2->n_points-1], 0);
    
    /* find the minimal distance between first or last point of both lines */
    mindistidx = 0;
    for (i = 0; i < 4; i++)
	if (distances[i] < distances[mindistidx])
	    mindistidx = i;

    G_debug (3, "merge line ? mindist: %g, thresh: %g",
	     distances[mindistidx], thresh);
    
    if (distances[mindistidx] > thresh) {
	return 0;
    }
    
    /* set index and other things */
    switch(mindistidx) {
	/* for each mindistidx create new line */
    case 0: 
	Vect_append_points (ps, Points2, GV_BACKWARD);
	Vect_append_points (ps, Points1, GV_FORWARD);
	break;
    case 1: 
	Vect_append_points (ps, Points2, GV_FORWARD);
	Vect_append_points (ps, Points1, GV_FORWARD);
	break;
    case 2: 
	Vect_append_points (ps, Points1, GV_FORWARD);
	Vect_append_points (ps, Points2, GV_FORWARD);
	break;
    case 3: 
	Vect_append_points (ps, Points1, GV_FORWARD);
	Vect_append_points (ps, Points2, GV_BACKWARD);
	break;
    default:
	break;
    }
    
    /* remove duplicate points */
    Vect_line_prune (ps);

    /* copy categories if needed */
    for (i = 0; i < Cats2 -> n_cats; i++) {
	Vect_cat_set (cs, Cats2 -> field[i], Cats2 -> cat[i]);
    }

    return 1;
}
