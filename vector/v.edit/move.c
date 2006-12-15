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
    int res = 0;


    /* cats or coord or bbox */
    if(cat_opt->answer != NULL) {
        
        res = move_categories(Map);
    }
    else if (coord_opt->answer != NULL) {

        res = move_coordinates(Map);
    }
    else if (bbox_opt->answer != NULL) {

        res = move_bbox(Map);
    }
    else {
        /* this case should not happen, see args.c for details */
        G_warning("cats, coord or bbox must be specified");
    }

}

int move_bbox(struct Map_info *Map) 
{
    double x1,y1,x2,y2;
    BOUND_BOX bbox;
    int cat, ret, type, i,j;
    struct ilist *List;
    SPATIAL_INDEX si;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int moved = 0;
    float move[2];
    

    /* bounding box */
    x1 = atof(bbox_opt->answers[0]);
    y1 = atof(bbox_opt->answers[1]);
    x2 = atof(bbox_opt->answers[2]);
    y2 = atof(bbox_opt->answers[3]);
    bbox.N = y1 < y2 ? y2 : y1;
    bbox.S = y1 < y2 ? y1 : y2;
    bbox.W = x1 < x2 ? x1 : x2;
    bbox.E = x1 < x2 ? x2 : x1;
    bbox.T = 0.0;
    bbox.B = 0.0;

    move[0] = atof(move_opt->answers[0]);
    move[1] = atof(move_opt->answers[1]);

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    List = Vect_new_list ();

    /* get lines number */
    Vect_select_lines_by_box ( Map, &bbox, GV_POINTS | GV_LINES | GV_BOUNDARY | GV_CENTROID, List );
    G_debug ( 1, "  %d lines selected by box", List->n_values );

    for ( i = 0; i < List->n_values; i++) {

        type = Vect_read_line(Map, Points, Cats, List->value[i]);
        G_debug(2, "Moving type [%d] number [%d]", type, List->value[i]);
        /* move */
        for (j = 0; j < Points->n_points; j++) {

            Points->x[j]+=move[0];
            Points->y[j]+=move[1];

        }/* /for each point at line */

        if ( Vect_rewrite_line (Map, List->value[i], type, Points, Cats) < 0)  {
            G_warning("Feature could not be moved");
            return 0;
        }
        moved++;
        /* attr_del(Map, layer, cat);*/
    }
    G_message(_("%d features moved"), moved);
    return 1;
}

int move_coordinates(struct Map_info *Map) 
{
    double east, north;
    int line,j;
    double maxdist = 0.5;
    char buff[16] = "";
    float move[2];

    int type;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int cat;
    int field=atoi(fld_opt->answer);
    int moved = 0;

    east = atof(coord_opt->answers[0]);
    north = atof(coord_opt->answers[1]);

    move[0] = atof(move_opt->answers[0]);
    move[1] = atof(move_opt->answers[1]);

    line = Vect_find_line(Map, east, north, 0.0, GV_POINT | GV_CENTROID, maxdist, 0, 0);

    if (line == 0) 
        line = Vect_find_line(Map, east, north, 0.0, GV_LINE | GV_BOUNDARY | GV_FACE, maxdist, 0, 0);


    G_debug (2, "line = %d", line);

    if (line == 0) {
        G_warning(_("Nothing Found."));
        return 0;

    }
    else {
	Points = Vect_new_line_struct();
	Cats = Vect_new_cats_struct();
        type = Vect_read_line(Map, Points, Cats, line);
        if ((cat = Vect_get_line_cat(Map, line, field )) > 0) 
            sprintf(buff,_("category [%d]"),cat);
            
        G_debug(2, "Moving type [%d] number [%d] %s", type, line, buff);
        /* move */
        for (j = 0; j < Points->n_points; j++) {

            Points->x[j]+=move[0];
            Points->y[j]+=move[1];

        }/* /for each point at line */

        if ( Vect_rewrite_line (Map, line, type, Points, Cats) < 0)  {
            G_warning("Feature could not be moved");
            return 0;
        }
        moved++;

        /* attr_del(Map, layer, cat);*/
    }

    G_message(_("%d features moved"), moved);
    return 1;
}


int move_categories(struct Map_info *Map) 
{
    
    struct cat_list * cl;
    int layer=atoi(fld_opt->answer);
    int i,j, ret, moved=0;
    float move[2];
    struct line_pnts *Points;
    struct line_cats *Cats;
    int field_idx;

    move[0] = atof(move_opt->answers[0]);
    move[1] = atof(move_opt->answers[1]);

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    cl = Vect_new_cat_list();
    Vect_str_to_cat_list(cat_opt->answer, cl);

    field_idx = Vect_cidx_get_field_index( Map, layer);

    G_debug(1,"layer = %d, field_idx = %d",layer, field_idx);

    for(i=0;i<cl->n_ranges;i++) {
	int cat, type, line;
	G_debug(1, "cl->min[%d]=%d, cl->max[%d]=%d, layer=%d",
		i, cl->min[i], i, cl->max[i], cl->field);
	for(cat=cl->min[i]; cat <= cl->max[i]; cat++) {
	    ret = Vect_cidx_find_next(Map, field_idx, cat, GV_POINTS|GV_LINES | GV_BOUNDARY | GV_CENTROID, 
				      0, &type, &line);
            if (ret < 0) {
                G_warning(_("Feature of category [%d] not found"), cat);
                return 0;
            }

            type = Vect_read_line(Map, Points, Cats, line);

            G_debug(2,"Moving type [%d] number [%d] cat [%d] about [%f,%f]", type, line, cat,move[0],move[1]);

            /* move */
            for (j = 0; j < Points->n_points; j++) {

                Points->x[j]+=move[0];
                Points->y[j]+=move[1];

            }/* /for each point at line */

            if ( Vect_rewrite_line (Map, line, type, Points, Cats) < 0)  {
                G_warning("Feature could not be moved");
                return 0;
            }

            moved ++;
	}
    }
    G_message(_("%d features moved"), moved);

    return 1;
}
