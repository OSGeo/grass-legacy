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

int do_move(struct Map_info *Map)
{
    struct ilist *List;
    int i, j;
    double move_x, move_y;
    int type;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int moved;
    int newline;

    moved = 0;

    move_x = atof(move_opt->answers[0]);
    move_y = atof(move_opt->answers[1]);

    /* select lines */
    List = select_lines(Map);

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    for (i = 0; i < List->n_values; i++) {

        type = Vect_read_line(Map, Points, Cats, List->value[i]);

        G_debug(2, "Moving type [%d] number [%d]", type, List->value[i]);

        /* move */
        for (j = 0; j < Points -> n_points; j++) {

            Points->x[j] += move_x;
            Points->y[j] += move_y;

        } /* for each point at line */

	newline = Vect_rewrite_line (Map, List->value[i], type, Points, Cats);

        if (newline < 0)  {
            G_warning("Feature [%d] could not be moved",
		      List->value[i]);
            return -1;
        }

        moved++;
        
        if (i_flg->answer) 
            fprintf(stdout, "%d%s", List->value[i], i < List->n_values-1 ? "," : "\n");
    }
        
    G_message(_("[%d] features moved"), moved);

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    Vect_destroy_list (List);

    return moved;
}
