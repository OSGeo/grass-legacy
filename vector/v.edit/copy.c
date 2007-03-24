/***************************************************************
 *
 * MODULE:       v.digit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *               
 * PURPOSE:    This module edits vector maps. It is inteded to be mainly
 * 	       used by the the new v.digit GUI.
 *
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/

#include <grass/glocale.h>
#include "global.h"

int do_copy (struct Map_info *Map)
{
    struct ilist *List;
    struct line_cats *Cats;
    struct line_pnts *Points;
    struct line_pnts *NPoints;
    int i, j;
    int type, cat;
    int layer;
    
    layer = atoi(fld_opt->answer);
    
    /* Select features */
    List = select_lines (Map);

    if (List->n_values <1) {
        G_warning(_("No lines found"),List->n_values);
        return 0;
    }

    Cats = Vect_new_cats_struct (); 
    Points = Vect_new_line_struct();
    NPoints = Vect_new_line_struct();
    Vect_reset_line(NPoints);

    /* for each line, make a copy */
    for ( i = 0; i < List->n_values; i++) {
        type = Vect_read_line(Map, Points, Cats, List->value[i]);
        G_debug(2, "Copying type [%d] number [%d]", type, List->value[i]);

        /* copy */
        for (j = 0; j < Points->n_points; j++) {

            Vect_append_point(NPoints, 
                            Points->x[j],Points->y[j], Points->z[j]);

        }/* /for each point at line */

        if ( Vect_write_line (Map, type, NPoints, Cats) < 0)  {
            G_warning("Feature [%d] could not be copyed",List->value[i]);
            return 0;
        }
        
        /* set new category to copyed image */
        if ((cat = Vect_get_line_cat (Map, List->value[i], layer)) > 0) {
            Vect_cat_set (Cats,layer, cat);
        }

        if (i_flg->answer) 
            fprintf(stdout,"%d%s", List->value[i], i < List->n_values-1 ? "," : "\n");
    }
    return 1;
}
