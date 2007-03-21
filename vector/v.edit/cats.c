/***************************************************************
 *
 * MODULE:       v.digit
 * 
 * AUTHOR(S):  GRASS Development Team
 *             Jachym Cepicky <jachym  les-ejk cz>
 *             Radim Blazek
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

#include "global.h"

/* cats 
 * edit category numbers of selected vector features
 * */
int cats (struct Map_info *Map, int del)
{
    int i, line;
    struct ilist *List;
    struct line_cats *Cats;
    struct line_pnts *Points;
    struct cat_list *Clist;
    int type;
    int cat ;
    int layer=atoi(fld_opt->answer);
    int field_idx;
    int ret;
    
    /* get list of categories */
    Clist = Vect_new_cat_list();
    if ( Vect_str_to_cat_list(cat_opt->answer, Clist)) 
        G_fatal_error(_("Could not get cat list <%s>"), cat_opt->answer);

    
    /* set resulting category number */
    cat = Clist->max[Clist->n_ranges-1];

    /* featuers defined by cats */
    if(cat_opt->answer != NULL && Clist->n_ranges > 1) {
        
        /* last category is the default one */
        int maxrange= Clist->n_ranges >= 2 ? Clist->n_ranges -1 : Clist->n_ranges;
        int tmpcat;

        if (maxrange >= 2)
            maxrange -=1;

        List = Vect_new_list ();
        field_idx = Vect_cidx_get_field_index( Map, layer );
        for(i=0;i<maxrange;i++) {
            for(tmpcat=Clist->min[i]; tmpcat <= Clist->max[i]; tmpcat++) {
                printf("%d\n",tmpcat);
                Vect_cidx_find_all(Map, field_idx,  GV_POINT | GV_CENTROID | GV_LINES | GV_BOUNDARY | GV_FACE, tmpcat,  List);
            }
        }
    }
    if (coord_opt->answer != NULL) {

        List = sel_by_coordinates(Map);
    }
    else if (bbox_opt->answer != NULL) {

        List = sel_by_bbox(Map);
    }
    else if (poly_opt->answer != NULL) {

        List = sel_by_polygon(Map);
    }
    else if (Clist->n_ranges < 2){
        /* this case should not happen, see args.c for details */
        G_fatal_error("cats, coord, bbox or polygon must be specified");
    }

    if (List->n_values <1) {
        G_warning(_("No lines found"),List->n_values);
        return 0;
    }

    Cats = Vect_new_cats_struct (); 
    Points = Vect_new_line_struct();

    /* for each line, set new category */
    for ( line = 0; line < List->n_values; line++ ) {
            type = Vect_read_line(Map, Points, Cats, List->value[line]);

        /* add new category */
        if (!del) {
            if((ret=Vect_cat_set (Cats,	layer, 	cat)) ==0) {
                G_warning(_("Could not set category [%d] to line number [%d]"), cat, List->value[line]);
                return 0;
            }
        }
        /* delete old category */
        else {
            if((ret=Vect_field_cat_del ( Cats, layer, cat)) == 0) {
                G_warning(_("Could not del category line  [%d]"), List->value[line]);
                return 0;
            }
        }

        if ((ret =Vect_rewrite_line (Map, List->value[line], type, Points, Cats) < 0) == -1)  {
            G_warning("Line could not be saved");
            return 0;
        }

        if (i_flg->answer) 
            fprintf(stdout,"%d%s", List->value[line], line < List->n_values-1 ? "," : "\n");
    }
    if (del) 
        G_message(_("Category [%d] and layer [%d] deleted from features"), cat,layer);
    else
        G_message(_("Category [%d] and layer [%d] set to selected features"), cat,layer);

    return 1;
}
